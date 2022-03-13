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

namespace Pansar.Tests

open System
open Xunit
open FsUnit.Xunit
open Pansar
open FsUnit.CustomMatchers
open System.Buffers


module ``eitherByte matcher`` =

  let toBytes (str: string) =
    if String.IsNullOrEmpty(str) then
      Array.empty
    elif str.IndexOf(';') = -1 then
      str |> Convert.ToByte |> Array.create 1
    else
      str.Split(';') |> Array.map byte

  module ``match`` =

    [<Theory>]
    [<InlineData("1;5;7", "1;2;3;4;5;6;7")>]
    let ``with matching sequence should register match`` (ofSeq: string) (input: string) =
      let ofSeq = toBytes ofSeq
      let inputSeq = toBytes input |> ReadOnlySequence

      let matcher = Match.eitherByte ofSeq : IMatcher

      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 1

    [<Theory>]
    [<InlineData("1;5;7", "2;3;4;6")>]
    [<InlineData("1;5;7", "2;1;5;7")>]
    let ``with non-matching sequence should not register match`` (ofSeq: string) (input: string) =
      let ofSeq = toBytes ofSeq
      let inputSeq = toBytes input |> ReadOnlySequence

      let matcher = Match.eitherByte ofSeq : IMatcher

      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.eitherByte [ 1uy ] : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal false
      tracker.Matches |> should equal 0

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.eitherByte [ 1uy; 2uy ] : IMatcher

      let mutable reader = SequenceReader([| 3uy |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, true)

      let startPos = reader.Position

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0
      reader.Position |> should equal startPos

  module matchMany =

    [<Theory>]
    [<InlineData("1;5;7", "1;5;1;7;2;2;2", "1;5;1;7", "2;2;2")>]
    [<InlineData("1;5;7", "1", "1", "")>]
    let ``with matching sequence should register match``
      (ofSeq: string)
      (input: string)
      (expectedConsumed: string)
      (expectedRemainings: string)
      =
      let ofSeq = toBytes ofSeq
      let inputSeq = toBytes input |> ReadOnlySequence
      let expectedConsumed = toBytes expectedConsumed
      let expectedRemainings = toBytes expectedRemainings
      let expectedMatchCount = expectedConsumed.Length

      let matcher = Match.eitherByte ofSeq : IMatcher

      let materializerFactory = singleByteMaterializer ()
      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = materializerFactory.MakeTracker(reader.Position)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)
      let (consumed, remainings) = resultParts materializerFactory tracker inputSeq

      hadEnoughData |> should equal true
      tracker.Matches |> should equal expectedMatchCount
      consumed |> seqEqual expectedConsumed
      remainings |> seqEqual expectedRemainings

    [<Fact>]
    let ``should position reader at latest match`` () =
      let expectedConsumed = [ 1uy; 1uy ]
      let expectedRemainings = [ 2uy ]

      let inputData = expectedConsumed @ expectedRemainings
      let inputSeq = inputData |> List.toArray |> ReadOnlySequence

      let matcher = Match.eitherByte [ 1uy ] : IMatcher

      let materializerFactory = singleByteMaterializer ()
      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = materializerFactory.MakeTracker(reader.Position)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)
      let (consumed, remainings) = resultParts materializerFactory tracker inputSeq

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 2
      consumed |> seqEqual expectedConsumed
      remainings |> seqEqual expectedRemainings


    [<Theory>]
    [<InlineData("1;5;7", "2;3;4;6")>]
    [<InlineData("1;5;7", "2;1;5;7")>]
    let ``with non-matching sequence should not register match`` (ofSeq: string) (input: string) =
      let ofSeq = toBytes ofSeq
      let inputSeq = toBytes input |> ReadOnlySequence

      let matcher = Match.eitherByte ofSeq : IMatcher

      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.eitherByte [ 1uy ] : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal false
      tracker.Matches |> should equal 0

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.eitherByte [ 1uy ] : IMatcher

      let mutable reader = SequenceReader([| 2uy |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, true)

      let startPos = reader.Position

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0
      reader.Position |> should equal startPos

  module findNext =

    [<Theory>]
    [<InlineData("5;1", "2;5;6;3;1;14", 5, 1)>]
    [<InlineData("12;13;14", "1;5;6;3;1;14", 14, 5)>]
    let ``with matching byte in sequence should register first match``
      (ofSeq: string)
      (input: string)
      (expectedMatch: byte)
      (pos: int64)
      =
      let ofSeq = toBytes ofSeq
      let inputSeq = toBytes input |> ReadOnlySequence

      let matcher = Match.eitherByte ofSeq : IMatcher

      let materializerFactory = singleByteMaterializer ()
      let mutable reader = SequenceReader(inputSeq)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)
      let consumed = materializeFindResult materializerFactory reader.Position length inputSeq

      result |> should equal FindResult.Found
      length |> should equal 1L
      consumed |> should equal expectedMatch
      reader.Consumed |> should equal pos

    [<Theory>]
    [<InlineData("5;1", "2;6;3;14")>]
    [<InlineData("12;13;14", "1;5;6;3;1")>]
    let ``with non-matching sequence should not register match`` (ofSeq: string) (input: string) =
      let ofSeq = toBytes ofSeq
      let inputSeq = toBytes input |> ReadOnlySequence

      let matcher = Match.eitherByte ofSeq : IMatcher

      let mutable reader = SequenceReader(inputSeq)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotFound

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.eitherByte [ 1uy ] : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotEnoughData

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.eitherByte [ 1uy ] : IMatcher

      let mutable reader = SequenceReader([| 2uy |] |> ReadOnlySequence)
      let mutable length = 0L

      let startPos = reader.Position

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotFound
      reader.Position |> should equal startPos
