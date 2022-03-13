(*

   Copyright 2022 The Pansar Authors.

   Licensed under the Apache License, Version 2.0 (the "License");
   you may not use this file except in compliance with the License.
   You may obtain a copy of the License at

       http://www.apache.org/licenses/LICENSE-2.0

   Unless required by applicable law or agreed to in writing, software
   distributed under the License is distributed on an "AS IS" BASIS,
   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
   See the License for the specific language governing permissions and
   limitations under the License.

*)

namespace Pansar

open System.Buffers
open System
open System.Runtime.CompilerServices
open Pansar.Buffers
open System.Linq

[<Struct>]
type Void = Void

/// Used by consumers to register matches when consuming multiple items.
[<Struct; IsByRefLike>]
type MatchTracker =

  val mutable StartPos: SequencePosition
  val mutable Matches: int
  val mutable Length: int64
  val mutable All: ResizeArray<int64>
  val mutable TrackIndividual: bool

  new(init: SequencePosition, trackIndividual: bool) =
    { StartPos = init
      Matches = 0
      Length = 0
      All =
        if trackIndividual then
          ResizeArray()
        else
          Unchecked.defaultof<ResizeArray<_>>
      TrackIndividual = trackIndividual }

  /// Register a match
  member this.Inc(len) = this.IncBy(len, 1)

  /// Register multiple matches atomically
  member this.IncBy(len, count) =
    this.Length <- this.Length + (len * int64 count)
    this.Matches <- this.Matches + count

    if this.TrackIndividual then
      if count = 1 then
        this.All.Add(len)
      else
        for _ in 0..count do
          this.All.Add(len)

/// A `IMaterializerConverter{T}` is used to convert a previous match (from bytes) to {T}
type IMaterializerConverter<'T> =

  /// The method that performs the conversion. The span is sliced and limited to only be
  /// the same bytes as the consumer register for the match.
  abstract member Materialize: inref<ReadOnlySpan<byte>> -> 'T

/// A `IMaterializerFunc{T}` is used to read a part of the supplied `SequenceReader`
/// and materializing the read part into a {T}
///
/// The SequenceReader is usually shared between multiple MaterializerFuncs and setup to be called in sequence.
type IMaterializerFunc<'T> =

  /// Uses the supplied `SequenceReader` to read a specific sequence of items and materializes it into `{T}`
  /// All `IMaterializerFunc{T}` are read in sequence, meaning that the func should not rewind/read backwards.
  ///
  /// The reader should in most cases be positioned correct but a func may need to advance to the expected start position.
  abstract member Materialize: inref<ReadOnlySequence<byte>> -> 'T

[<AbstractClass>]
type MaterializerFactory<'T0, 'T1>(trackIndividual: bool) =

  member __.MakeTracker(init) = MatchTracker(init, trackIndividual)

  abstract member Single: SequencePosition * int64 -> IMaterializerFunc<'T0>
  abstract member Many: inref<MatchTracker> -> IMaterializerFunc<'T1>

module MaterializerConverter =

  /// Read out a slice in the length of `length`, starting at the given `pos`, and then run the `IMaterializerConverter{}`
  /// on the read span
  let inline run
    (converter: IMaterializerConverter<_>)
    (sequence: inref<ReadOnlySequence<byte>>)
    (pos: SequencePosition)
    (length: int64)
    =
    let slice = sequence.Slice(pos, length)

    let span =
      if slice.IsSingleSegment then
        slice.FirstSpan
      else
        Span.asReadOnly ((slice.ToArray()).AsSpan())

    converter.Materialize(&span)

  /// Combine two converters. The span used for materializing is sliced based on the
  /// given `len` parameter.
  ///
  /// `a` will be given a span from the start till the inclusive len
  /// `b` will be given the rest of the span
  let inline combine (a: IMaterializerConverter<_>) (b: IMaterializerConverter<_>) len =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) =
          let first = span.Slice(0, len)
          let second = span.Slice(len)

          (a.Materialize(&first), b.Materialize(&second)) }

module MaterializerFunc =

  /// Apply the `IMaterializerFunc{}` on the sequence
  let inline run (func: IMaterializerFunc<_>) (seq: inref<ReadOnlySequence<byte>>) = func.Materialize &seq

  /// Create a new `MaterializerFunc{}` that, when called during materialization, applies the converter on a segment starting at `startPos` and is `length` long
  let inline scoped (converter: IMaterializerConverter<_>) (startPos: SequencePosition) length =
    { new IMaterializerFunc<_> with
        override __.Materialize(reader) =
          MaterializerConverter.run converter &reader startPos length }

  /// Create a new `MaterializerFunc{}` that always materializes into the `value`
  let inline get value =
    { new IMaterializerFunc<_> with
        override __.Materialize(_) = value }

  /// Create a new `MaterializerFunc{}` that will run `f` on the materialized value from the inner `func`
  let inline map ([<InlineIfLambda>] f) (func: IMaterializerFunc<_>) =
    { new IMaterializerFunc<_> with
        override __.Materialize(seq) =
          let result = func.Materialize(&seq)
          result |> f }

  /// Create a new `MaterializerFunc{}` that combines the result from `a` and `b` in a tuple
  let inline combine (a: IMaterializerFunc<_>) (b: IMaterializerFunc<_>) =
    { new IMaterializerFunc<_> with
        override __.Materialize(seq) =
          (a.Materialize(&seq), b.Materialize(&seq)) }

  /// Create a new `MaterializerFunc{}` that combines the result from `a`, `b` and `c` in a 3-tuple
  let inline combine3 (a: IMaterializerFunc<_>) (b: IMaterializerFunc<_>) (c: IMaterializerFunc<_>) =
    { new IMaterializerFunc<_> with
        override __.Materialize(seq) =
          (a.Materialize(&seq), b.Materialize(&seq), c.Materialize(&seq)) }

  /// Create a new `MaterializerFunc{}` that runs all `materializers` and combines the result in a list
  let inline combineMany (materializers: IMaterializerFunc<_> list) =
    { new IMaterializerFunc<_> with
        override __.Materialize(reader) =
          let materializers = List.toArray materializers

          let len = materializers.Length
          let ixLen = len - 1

          let mutable results = Array.zeroCreate len

          for i = ixLen downto 0 do
            results[ixLen - i] <- materializers[ i ].Materialize(&reader)

          results }

[<AutoOpen>]
module MaterializerFactoryTypes =

  /// `SquashMaterializer{}` will only track the first and the latest match for a consumer. The materialization will be applied
  /// on the whole range.
  ///
  /// Suits well for types that in its naturally form is a sequence of items. For example string that is a collection of char. Instead of materializing
  /// into a char[], the `SquashMaterializer{}` would materialize the sequence of chars into a `string`
  type SquashMaterializer<'T>(converter: IMaterializerConverter<'T>) =
    inherit MaterializerFactory<'T, 'T>(false)

    override __.Single(startPos, len) =
      MaterializerFunc.scoped converter startPos len

    override __.Many(tracker: inref<MatchTracker>) =
      MaterializerFunc.scoped converter tracker.StartPos tracker.Length

  /// `StepMaterializer{}` will track all matches from a consumer, and run the materalizer on all matches. The result from the materialization is an array of `{T}`
  type StepMaterializer<'T>(converter: IMaterializerConverter<'T>) =
    inherit MaterializerFactory<'T, 'T array>(true)

    override __.Single(startPos, len) =
      MaterializerFunc.scoped converter startPos len

    override __.Many(tracker: inref<MatchTracker>) =

      let init = tracker.StartPos
      let matches = tracker.Matches
      let total = tracker.Length
      let all = tracker.All

      { new IMaterializerFunc<_> with
          override __.Materialize(seq) =
            let mutable reader = SequenceReader(seq.Slice(init, total))

            let result = Array.zeroCreate matches

            let maxLen = all.Max() |> int

            let buffer =
              if maxLen <= 256 then
                Span.stackalloc maxLen
              else
                (Array.zeroCreate maxLen).AsSpan()

            for i in 0 .. matches - 1 do
              let itemLength = int all[i]

              let span =
                if buffer.Length <> itemLength then
                  buffer.Slice(itemLength)
                else
                  buffer

              if reader.TryCopyTo(span) then
                let mutable readOnly = Span.asReadOnly span
                result[i] <- converter.Materialize(&readOnly)

              reader.Advance(itemLength)

            result }

  /// `VoidMaterializer{}` will always create a materializer func that materializes to void.
  type VoidMaterializer() =
    inherit MaterializerFactory<Void, Void>(false)

    override __.Single(_, _) = MaterializerFunc.get Void

    override __.Many(_) = MaterializerFunc.get Void

module MaterializerFactory =

  /// Create a new materializer factory with a squash strategy
  let inline squash converter = SquashMaterializer(converter)

  /// Create a new materializer factory with a step strategy
  let inline step converter = StepMaterializer(converter)

module Materialize =

  // Do not convert the data into a result, just use a `Void` placeholder.
  let ignore () = VoidMaterializer()
