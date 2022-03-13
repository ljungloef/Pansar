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

open Xunit
open FsUnit.Xunit
open Pansar
open FsUnit.CustomMatchers
open System.Buffers


module ``ifByte matcher`` =

  let toPredicate (b : byte) = fun v -> v = b

  module ``match`` =

    [<Theory>]
    [<InlineData(1uy)>]
    [<InlineData(241uy)>]
    [<InlineData(0uy)>]
    let ``with matching sequence should register match`` (input: byte) =
      let matcher = Match.ifOne (toPredicate input) : IMatcher

      let mutable reader = SequenceReader([| input |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 1

    [<Theory>]
    [<InlineData(1uy, 2uy)>]
    [<InlineData(241uy, 2uy)>]
    [<InlineData(0uy, 2uy)>]
    let ``with non-matching sequence should not register match`` (input: byte) (seq: byte) =
      let matcher = Match.ifOne (toPredicate input) : IMatcher

      let mutable reader = SequenceReader([| seq |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.ifOne (toPredicate 1uy) : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal false
      tracker.Matches |> should equal 0

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.ifOne (toPredicate 1uy) : IMatcher

      let mutable reader = SequenceReader([| 2uy |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, true)

      let startPos = reader.Position

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0
      reader.Position |> should equal startPos

  module ``matchMany`` =

    [<Theory>]
    [<InlineData(1uy)>]
    [<InlineData(241uy)>]
    [<InlineData(0uy)>]
    let ``with matching sequence should register match`` (input: byte) =
      let count = 10

      let inputData = Array.create count input
      let inputSeq = inputData |> ReadOnlySequence

      let matcher = Match.ifOne (toPredicate input) : IMatcher

      let materializerFactory = singleByteMaterializer ()
      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = materializerFactory.MakeTracker(reader.Position)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)
      let (consumed, remainings) = resultParts materializerFactory tracker inputSeq

      hadEnoughData |> should equal true
      tracker.Matches |> should equal count
      consumed |> seqEqual inputData
      remainings |> should haveLength 0

    [<Fact>]
    let ``should position reader at latest match`` () =
      let expectedConsumed = [ 1uy; 1uy ]
      let expectedRemainings = [ 2uy ]

      let inputData = expectedConsumed @ expectedRemainings
      let inputSeq = inputData |> List.toArray |> ReadOnlySequence

      let matcher = Match.ifOne (toPredicate 1uy) : IMatcher

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
    [<InlineData(1uy, 2uy)>]
    [<InlineData(241uy, 2uy)>]
    [<InlineData(0uy, 2uy)>]
    let ``with non-matching sequence should not register match`` (input: byte) (seq: byte) =
      let matcher = Match.ifOne (toPredicate input) : IMatcher

      let mutable reader = SequenceReader([| seq |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.ifOne (toPredicate 1uy) : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable tracker = MatchTracker(reader.Position, true)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal false
      tracker.Matches |> should equal 0

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.ifOne (toPredicate 1uy) : IMatcher

      let mutable reader = SequenceReader([| 2uy |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, true)

      let startPos = reader.Position

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0
      reader.Position |> should equal startPos

  module ``findNext`` =

    [<Theory>]
    [<InlineData(1uy, "2;5;6;3;1;14", 4)>]
    [<InlineData(1uy, "1;5;6;3;1;14", 0)>]
    [<InlineData(1uy, "2;5;6;3;1;14;1", 4)>]
    let ``with matching byte in sequence should register first match`` (input: byte) (data: string) (pos : int64) =
      let inputSeq = data.Split(';') |> Array.map byte |> ReadOnlySequence
      let matcher = Match.ifOne (toPredicate input) : IMatcher

      let mutable reader = SequenceReader(inputSeq)
      let mutable length = 0L

      let materializerFactory = singleByteMaterializer ()
      let result = matcher.FindNext(&reader, &length)
      let consumed = materializeFindResult materializerFactory reader.Position length inputSeq

      result |> should equal FindResult.Found
      length |> should equal 1L
      consumed |> should equal input
      reader.Consumed |> should equal pos

    [<Theory>]
    [<InlineData(13uy, "2;5;6;3;1;14")>]
    [<InlineData(241uy, "2;5;6;3;1;14")>]
    [<InlineData(0uy, "2;5;6;3;1;14")>]
    let ``with non-matching sequence should not register match`` (input: byte) (data: string) =
      let inputSeq = data.Split(';') |> Array.map byte |> ReadOnlySequence
      let matcher = Match.ifOne (toPredicate input) : IMatcher

      let mutable reader = SequenceReader(inputSeq)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotFound

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.ifOne (toPredicate 1uy) : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotEnoughData

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.ifOne (toPredicate 1uy) : IMatcher

      let mutable reader = SequenceReader([| 2uy |] |> ReadOnlySequence)
      let mutable length = 0L

      let startPos = reader.Position

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotFound
      reader.Position |> should equal startPos