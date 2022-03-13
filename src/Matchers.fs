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
open Pansar.Buffers

/// Used to test and match the underlying byte values
type IMatcher =

  inherit IDelimiterSearcher

  /// Tries to match once at the current position in the provided `System.Buffers.SequenceReader{byte}`.
  /// The consumer registers the match in the `MatchTracker`.
  ///
  /// Returns 'true` if the sequence contained enough items to do the search; otherwise `false`.
  abstract member Match: byref<SequenceReader<byte>> * byref<MatchTracker> -> bool

  /// Tries to match multiple times, starting at the current position in the provided `System.Buffers.SequenceReader{byte}`.
  /// The consumer registers all matches in the `MatchTracker`.
  ///
  /// Returns 'true` if the sequence contained enough items to do the search; otherwise `false`. The consumer will only return `false` if the first match
  /// fails because of insufficient amount of items.
  abstract member MatchMany: byref<SequenceReader<byte>> * byref<MatchTracker> -> bool

and IDelimiterSearcher =

  /// Tries to find the next match in a `System.Buffers.ReadOnlySequence{byte}`, using the provided `System.Buffers.SequenceReader{byte}`.
  /// The search starts from the reader's current position.
  ///
  /// Returns `FindResult.Found` and sets the `length` parameter to the delimiter's length if the delimiter was found. The reader will be positioned at the start of the delimiter.
  /// If the delimiter was not found, the searcher returns 'FindResult.NotFound` if the sequence contained enough items to do the search; otherwise `FindResult.NotEnoughData`.
  abstract member FindNext: byref<SequenceReader<byte>> * outref<int64> -> FindResult

/// Used by a matcher to report the result when searching through a sequence of items.
and FindResult =
  // The search could not be started, because the sequence did not contain enough items to do the search.
  | NotEnoughData = 0uy

  // The search was carried out to the end of the sequence, but no match was found.
  | NotFound = 1uy

  // Match found.
  | Found = 2uy

/// Defines a predicate that needs more than 1 byte to test values. The size can be defined using the `MinLength` and `MaxLength`.
type IRangePredicate =

  /// The minimum length required by the `Predicate` to test values.
  abstract member MinLength: int64

  /// Max limit. Can be used to indicate the size of the underlying span slice.
  abstract member MaxLength: int64

  /// The actual predicate that tests the values.
  abstract member Predicate: ISpanPredicate

module Matching =

  let inline return' () = true

  let inline tryFindNextSingleByte (reader: byref<SequenceReader<byte>>) delimiter (length: outref<int64>) =
    if reader.TryAdvanceTo(delimiter, false) then
      length <- 1
      FindResult.Found
    else
      FindResult.NotFound

  let inline tryFindNextOf
    (reader: byref<SequenceReader<byte>>)
    (delimiter: ReadOnlySpan<byte>)
    (length: outref<int64>)
    =
    if reader.TryAdvanceToAny(delimiter, false) then
      length <- 1
      FindResult.Found
    else
      FindResult.NotFound

  let inline tryFindNextIf
    (reader: byref<SequenceReader<byte>>)
    ([<InlineIfLambda>] predicate: byte -> bool)
    (length: outref<int64>)
    =
    if reader.TryAdvanceTo(predicate, false) then
      length <- 1
      FindResult.Found
    else
      FindResult.NotFound

  let inline tryFindNextIfRanged
    (reader: byref<SequenceReader<byte>>)
    (definition: IRangePredicate)
    (actualLength: outref<int64>)
    =
    if reader.TryAdvanceTo(definition.Predicate, definition.MinLength, definition.MaxLength, &actualLength, false) then
      FindResult.Found
    else
      FindResult.NotFound

  let inline tryFindNextSeq
    (reader: byref<SequenceReader<byte>>)
    (delimiter: ReadOnlySpan<byte>)
    (length: outref<int64>)
    =
    let mutable __ = ReadOnlySequence<byte>.Empty

    if reader.TryReadTo(&__, delimiter, false) then
      length <- delimiter.Length
      FindResult.Found
    else
      FindResult.NotFound

  let inline matchOnceSingleByte (reader: byref<SequenceReader<byte>>) (lookFor: byte) (tracker: byref<MatchTracker>) =
    if reader.IsNext(lookFor, true) then
      tracker.Inc(1)

  let inline matchManySingleByte (reader: byref<SequenceReader<byte>>) (lookFor: byte) (tracker: byref<MatchTracker>) =
    while reader.IsNext(lookFor, true) do
      tracker.Inc(1)

  let inline matchManyByLen (reader: byref<SequenceReader<byte>>) len (tracker: byref<MatchTracker>) =
    let count = Math.Floor(reader.Remaining / len) |> int64
    let consumed = len * count

    reader.Advance(consumed)
    tracker.IncBy(len, int count)

  let inline matchManySeq
    (reader: byref<SequenceReader<byte>>)
    (lookFor: ReadOnlySpan<byte>)
    (tracker: byref<MatchTracker>)
    =
    while reader.IsNext(lookFor, true) do
      tracker.Inc(lookFor.Length)

  let inline matchOnceSeq
    (reader: byref<SequenceReader<byte>>)
    (lookFor: ReadOnlySpan<byte>)
    (tracker: byref<MatchTracker>)
    =
    if reader.IsNext(lookFor, true) then
      tracker.Inc(lookFor.Length)

  let inline matchOnceByLen (reader: byref<SequenceReader<byte>>) len (tracker: byref<MatchTracker>) =
    reader.Advance(len)
    tracker.Inc(len)

  let inline matchManyOf (reader: byref<SequenceReader<byte>>) anyOf (tracker: byref<MatchTracker>) =
    while reader.IsAnyNext(anyOf, true) do
      tracker.Inc(1)

  let inline matchManyIf
    (reader: byref<SequenceReader<byte>>)
    ([<InlineIfLambda>] predicate: byte -> bool)
    (tracker: byref<MatchTracker>)
    =
    while SequenceReader.canRead &reader
          && reader.IsNext(predicate, true) do
      tracker.Inc(1)

  let inline matchManyIfRanged
    (reader: byref<SequenceReader<byte>>)
    (definition: IRangePredicate)
    (tracker: byref<MatchTracker>)
    =
    let mutable actualLengh = Unchecked.defaultof<int64>

    while SequenceReader.canRead &reader
          && reader.IsNext(definition.Predicate, definition.MinLength, definition.MaxLength, &actualLengh, true) do
      tracker.Inc(actualLengh)

  let inline matchOnceOf
    (reader: byref<SequenceReader<byte>>)
    (anyOf: ReadOnlySpan<byte>)
    (tracker: byref<MatchTracker>)
    =
    if reader.IsAnyNext(anyOf, true) then
      tracker.Inc(1)

  let inline matchOnceIf
    (reader: byref<SequenceReader<byte>>)
    ([<InlineIfLambda>] predicate: byte -> bool)
    (tracker: byref<MatchTracker>)
    =
    if reader.IsNext(predicate, true) then
      tracker.Inc(1)

  let inline matchOnceIfRanged
    (reader: byref<SequenceReader<byte>>)
    (definition: IRangePredicate)
    (tracker: byref<MatchTracker>)
    =
    let mutable actualLength = Unchecked.defaultof<int64>

    if reader.IsNext(definition.Predicate, definition.MinLength, definition.MaxLength, &actualLength, true) then
      tracker.Inc(actualLength)

module Matchers =

  open Matching

  /// The `ByteEqMatcher` is a single byte matcher and matches on a byte equal to the `Eq` value.
  type ByteEqMatcher(eq: byte) =

    member __.Eq = eq

    interface IMatcher with

      override __.MatchMany(reader, tracker) =
        SequenceReader.canRead &reader
        && return' (matchManySingleByte &reader eq &tracker)

      override __.Match(reader, tracker) =
        SequenceReader.canRead &reader
        && return' (matchOnceSingleByte &reader eq &tracker)

    interface IDelimiterSearcher with

      override __.FindNext(reader, length) =
        if SequenceReader.canRead &reader then
          tryFindNextSingleByte &reader eq &length
        else
          FindResult.NotEnoughData


  /// The `ByteSeqMatcher` is a byte matcher that matches on a sequence of bytes. All bytes in the seq must match in order.
  /// E.q. the seq [0;1;2] will match when comparing to [0;1;2;3], and unmatch on [0;1;3;2]
  type ByteSeqMatcher(seq: byte array) =

    let length = seq.LongLength

    member __.Seq = seq

    interface IMatcher with

      override this.MatchMany(reader, tracker) =
        SequenceReader.atLeast &reader length
        && return' (matchManySeq &reader (ReadOnlySpan(this.Seq)) &tracker)

      override this.Match(reader, tracker) =
        SequenceReader.atLeast &reader length
        && return' (matchOnceSeq &reader (ReadOnlySpan(this.Seq)) &tracker)

    interface IDelimiterSearcher with

      override this.FindNext(reader, lengthResult) =
        if SequenceReader.atLeast &reader length then
          tryFindNextSeq &reader (ReadOnlySpan(this.Seq)) &lengthResult
        else
          FindResult.NotEnoughData

  /// The `EitherByteMatcher` is a single byte consumer that matches if a byte is equal to any of the `Of` bytes.
  type EitherByteMatcher(set: byte seq) =

    let allowed = set |> Seq.toArray

    member __.Of = set

    interface IMatcher with

      override __.MatchMany(reader, tracker) =
        SequenceReader.canRead &reader
        && return' (matchManyOf &reader (ReadOnlySpan(allowed)) &tracker)

      override __.Match(reader, tracker) =
        SequenceReader.canRead &reader
        && return' (matchOnceOf &reader (ReadOnlySpan(allowed)) &tracker)

    interface IDelimiterSearcher with

      override __.FindNext(reader, length) =
        if SequenceReader.canRead &reader then
          tryFindNextOf &reader (ReadOnlySpan(allowed)) &length
        else
          FindResult.NotEnoughData


  /// The `IfByteMatcher` is a single byte consumer that will use the `Predicate` function to evaluate if the byte is a match or unmatch.
  type IfByteMatcher([<InlineIfLambda>] f: byte -> bool) =

    member __.Predicate = f

    interface IMatcher with

      override __.MatchMany(reader, tracker) =
        SequenceReader.canRead &reader
        && return' (matchManyIf &reader f &tracker)

      override __.Match(reader, tracker) =
        SequenceReader.canRead &reader
        && return' (matchOnceIf &reader f &tracker)

    interface IDelimiterSearcher with

      override __.FindNext(reader, length) =
        if SequenceReader.canRead &reader then
          tryFindNextIf &reader f &length
        else
          FindResult.NotEnoughData

  /// The `IfBytesMatcher` is a multi byte matcher that will use the `Predicate` definition to evaluate if the bytes is a match or unmatch.
  type IfBytesMatcher(definition: IRangePredicate) =

    member __.Predicate = definition

    interface IMatcher with

      override __.MatchMany(reader, tracker) =
        SequenceReader.atLeast &reader definition.MinLength
        && return' (matchManyIfRanged &reader definition &tracker)

      override __.Match(reader, tracker) =
        SequenceReader.atLeast &reader definition.MinLength
        && return' (matchOnceIfRanged &reader definition &tracker)

    interface IDelimiterSearcher with

      override __.FindNext(reader, length) =
        if SequenceReader.atLeast &reader definition.MinLength then
          tryFindNextIfRanged &reader definition &length
        else
          FindResult.NotEnoughData

  /// The `LengthMatcher` is a 0..N length byte matcher that will match if the sequence contains at minimum `Length` items; otherwise unmatch.
  type LengthMatcher(length) =

    let len = int length

    interface IMatcher with

      override __.MatchMany(reader, tracker) =
        SequenceReader.atLeast &reader len
        && return' (matchManyByLen &reader len &tracker)

      override __.Match(reader, tracker) =
        SequenceReader.atLeast &reader len
        && return' (matchOnceByLen &reader len &tracker)

    interface IDelimiterSearcher with

      override __.FindNext(reader, len) =
        if SequenceReader.atLeast &reader length then
          len <- length
          FindResult.Found
        else
          FindResult.NotEnoughData


[<RequireQualifiedAccess>]
module Match =

  open Matchers

  /// Creates a single byte consumer and matches on a byte equal to the `eq` value.
  let inline byte eq = ByteEqMatcher(eq)

  /// Creates a byte consumer that matches on a sequence of bytes. All bytes in the seq must match in order.
  let inline byteSequence seq = ByteSeqMatcher(seq |> Seq.toArray)

  /// Creates a single byte consumer that matches if a byte is equal to any of the `set` bytes.
  let inline eitherByte set = EitherByteMatcher(set)

  /// Creates a single byte consumer that will use the `f` function to evaluate if the byte is a match or unmatch.
  let inline ifOne ([<InlineIfLambda>] f: byte -> bool) = IfByteMatcher(f)

  /// Creates a M..N byte consumer that will use the `definition` to evaluate whether a range of bytes is a match or unmatch.
  let inline ifRange definition = IfBytesMatcher(definition)

  /// Creates a 0..N length byte consumer that will match if the sequence contains at minimum `size` items; otherwise unmatch.
  let inline bySize size = LengthMatcher(size)

  module IfMatcher =

    type IfMatcher = IfMatcher
      with
        /// Char matcher that only requires one byte. Can use the simplified parser that takes a single byte predicate
        static member ($)(_: IfMatcher, (predicate: byte -> bool)) = ifOne predicate

        /// Char matcher that can match on a sequence of bytes. To be used with char sets that is larger than 1 byte, e.g. UTF-8
        static member ($)(_: IfMatcher, (definition: IRangePredicate)) = ifRange definition

  open IfMatcher

  /// Helper function with parametric polymorphism.
  /// The `Match.ifOne` will be called if the `predicate` argument is a function with the signature `byte -> bool`.
  /// The `Match.ifRange` will be called if the `predicate` argument is a `IRangePredicate`
  let inline if' predicate = IfMatcher $ predicate
