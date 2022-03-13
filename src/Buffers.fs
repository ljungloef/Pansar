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

namespace Pansar.Buffers

open System.Buffers
open System
open System.Runtime.CompilerServices
open System.Runtime.InteropServices

#nowarn "9" "3391"

[<RequireQualifiedAccess>]
module Span =

  open Microsoft.FSharp.NativeInterop

  /// Uses the implicit cast from Span -> ReadOnlySpan
  let inline asReadOnly (span: Span<'a>) : ReadOnlySpan<'a> = span

  /// Forms a slice out of the current span, starting at index `start` and is `length` long.
  let inline sliceFrom (start: int) (length: int) (span: Span<_>) = span.Slice(start, length)

  /// Allocates memory on the stack
  let inline stackalloc len =
    let mem =
      NativePtr.stackalloc<byte> (len)
      |> NativePtr.toVoidPtr

    Span<byte>(mem, len)

[<RequireQualifiedAccess>]
module ReadOnlySpan =

  /// Returns the length of the span
  let inline len (span: ReadOnlySpan<_>) = span.Length

  /// Checks if the span contains at least `len` items
  let inline atLeast (span: ReadOnlySpan<_>) length = span.Length >= length


[<RequireQualifiedAccess>]
module ReadOnlySequence =

  /// Converts the sequence into an array
  let inline toArray (seq: ReadOnlySequence<_>) = seq.ToArray()

  /// The end position of the sequence
  let inline endPos (seq: ReadOnlySequence<_>) = seq.End

  /// Forms a slice out of the current sequence, starting at `pos` and is `length` long
  let inline sliceFrom (pos: SequencePosition) (length: int) (seq: ReadOnlySequence<_>) = seq.Slice(pos, length)

  /// Forms a slice out of the current sequence, starting at `pos` and includes all other items to the end.
  let inline sliceAt (pos: SequencePosition) (seq: ReadOnlySequence<_>) = seq.Slice(pos)

  /// Forms a s slice out of the current sequence, starting at `startPos` and ends at `endPos`
  let inline sliceBetween (startPos: SequencePosition) (endPos: SequencePosition) (seq: ReadOnlySequence<_>) =
    seq.Slice(startPos, endPos)

  /// Creates a new `SequenceReader` for the given sequence
  let inline reader (seq: ReadOnlySequence<_>) = SequenceReader(seq)

  /// Allocates memory on the stack and copies from the sequence to the newly allocated span
  let inline copyToStackalloc (seq: ReadOnlySequence<_>) len =
    let target = Span.stackalloc len
    seq.CopyTo(target)
    target

[<RequireQualifiedAccess>]
module SequenceReader =

  /// The reader has at least `len` unread items
  let inline atLeast (reader: byref<SequenceReader<byte>>) len = reader.Remaining >= len

  /// The reader has at least one item to read
  let inline canRead (reader: byref<SequenceReader<byte>>) = not reader.End

  /// Creates a new `SequenceReader` over the seq
  let inline fromSeq (seq: ReadOnlySequence<_>) = SequenceReader(seq)

  /// Create a new `SequenceReader` over a slice out of the `parent`'s sequence
  let inline sub (parent: byref<SequenceReader<_>>) (startPos: SequencePosition) (endPos: SequencePosition) =
    let seq =
      parent.Sequence
      |> ReadOnlySequence.sliceBetween startPos endPos

    fromSeq seq

type ISpanPredicate =
  abstract member IsMatch: ReadOnlySpan<byte> * outref<int64> -> bool

[<Extension>]
type SequenceReaderExtensions() =

  [<Extension>]
  /// Tests if any of the `anyOf` values is next in the sequence.
  static member IsAnyNext
    (
      reader: byref<SequenceReader<byte>>,
      anyOf: ReadOnlySpan<byte>,
      [<Optional; DefaultParameterValue(false)>] advancePast: bool
    ) =
    if reader.End then
      false
    else
      let next = reader.CurrentSpan[reader.CurrentSpanIndex]
      let isNext = anyOf.IndexOf(next) >= 0

      if isNext && advancePast then
        reader.Advance(1)

      isNext

  [<Extension>]
  /// Uses the `predicate` function to test the next value in sequence.
  static member IsNext
    (
      reader: byref<SequenceReader<byte>>,
      predicate: byte -> bool,
      [<Optional; DefaultParameterValue(false)>] advancePast: bool
    ) =
    if reader.End then
      false
    else
      let next = reader.CurrentSpan[reader.CurrentSpanIndex]
      let isNext = predicate next

      if isNext && advancePast then
        reader.Advance(1)

      isNext

  [<Extension>]
  /// Uses the `predicate` function to test the next value in sequence.
  static member IsNext
    (
      reader: byref<SequenceReader<byte>>,
      predicate: ISpanPredicate,
      min,
      max,
      length: outref<int64>,
      [<Optional; DefaultParameterValue(false)>] advancePast: bool
    ) =
    if reader.End || reader.Remaining < min then
      false
    else
      length <-
        if reader.Remaining < max then
          reader.Remaining
        else
          max

      let next = reader.CurrentSpan.Slice(reader.CurrentSpanIndex, int length)
      let isNext = predicate.IsMatch(next, &length)

      if isNext && advancePast then
        reader.Advance(length)

      isNext

  [<Extension>]
  /// Uses the `predicate` function to search for a specific delimiter in the sequence.
  static member TryAdvanceTo
    (
      reader: byref<SequenceReader<byte>>,
      predicate: byte -> bool,
      [<Optional; DefaultParameterValue(false)>] advancePastDelimiter: bool
    ) =
    let copy = reader

    let mutable found = false

    while not reader.End && not found do
      found <- reader.IsNext(predicate)

      if not found || advancePastDelimiter then
        reader.Advance(1)

    if not found then reader <- copy

    found

  [<Extension>]
  /// Uses the `predicate` function to search for a specific delimiter in the sequence.
  static member TryAdvanceTo
    (
      reader: byref<SequenceReader<byte>>,
      predicate: ISpanPredicate,
      min,
      max,
      actualLength: outref<int64>,
      [<Optional; DefaultParameterValue(false)>] advancePastDelimiter: bool
    ) =
    let copy = reader

    let mutable found = false

    while reader.Remaining >= min && not found do
      found <- reader.IsNext(predicate, min, max, &actualLength)

      if not found || advancePastDelimiter then
        reader.Advance(actualLength)

    if not found then reader <- copy

    found
