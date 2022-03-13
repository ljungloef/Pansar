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


module ``byteSeq matcher`` =

  module ``match`` =

    [<Theory>]
    [<InlineData("ab", "abcdef", "cdef")>]
    [<InlineData("a", "abcdef", "bcdef")>]
    [<InlineData("abcde", "abcdef", "f")>]
    [<InlineData("abcdef", "abcdef", "")>]
    let ``with matching sequence should register match``
      (lookFor: string)
      (input: string)
      (expectedRemainings: string)
      =
      let inputSeq = sequenceOf input

      let matcher = Match.byteSequence (utf8Bytes lookFor) : IMatcher

      let materializerFactory = stringMaterializer ()
      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = materializerFactory.MakeTracker(reader.Position)

      let hadEnoughData = matcher.Match(&reader, &tracker)
      let (consumed, remainings) = resultParts materializerFactory tracker inputSeq

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 1
      consumed |> should equal lookFor

      remainings
      |> seqEqual (utf8Bytes expectedRemainings)

    [<Theory>]
    [<InlineData("klmn", "abcdef")>]
    [<InlineData("r", "abcdef")>]
    let ``with non-matching sequence should not register match`` (lookFor: string) (input: string) =
      let matcher = Match.byteSequence (utf8Bytes lookFor) : IMatcher

      let mutable reader = SequenceReader(sequenceOf input)
      let mutable tracker = MatchTracker(reader.Position, false)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.byteSequence [| 22uy; 44uy; 66uy |] : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable tracker = MatchTracker(reader.Position, false)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal false
      tracker.Matches |> should equal 0

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.byteSequence [| 1uy |] : IMatcher

      let mutable reader = SequenceReader([| 2uy |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, false)

      let startPos = reader.Position

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0
      reader.Position |> should equal startPos

  module matchMany =

    [<Theory>]
    [<InlineData("abc", "abcabcabcdef", "abcabcabc", 3, "def")>]
    [<InlineData("a", "aaaaabcdef", "aaaaa", 5, "bcdef")>]
    [<InlineData("abcde", "abcdeabcde", "abcdeabcde", 2, "")>]
    [<InlineData("abc", "abcdefg", "abc", 1, "defg")>]
    let ``with matching sequence should register match``
      (lookFor: string)
      (input: string)
      (expectedConsumed: string)
      (expectedMatchCount: int)
      (expectedRemainings: string)
      =
      let inputSeq = sequenceOf input

      let matcher = Match.byteSequence (utf8Bytes lookFor) : IMatcher

      let materializerFactory = stringMaterializer ()
      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = materializerFactory.MakeTracker(reader.Position)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)
      let (consumed, remainings) = resultParts materializerFactory tracker inputSeq

      hadEnoughData |> should equal true
      tracker.Matches |> should equal expectedMatchCount
      consumed |> should equal expectedConsumed

      remainings
      |> seqEqual (utf8Bytes expectedRemainings)

    [<Fact>]
    let ``should position reader at latest match`` () =
      let lookFor = [ 1uy; 3uy ]
      let expectedConsumed = lookFor @ lookFor
      let expectedRemainings = [ 2uy ]

      let inputData = expectedConsumed @ expectedRemainings
      let inputSeq = inputData |> List.toArray |> ReadOnlySequence

      let materializerFactory = byteMaterializer ()
      let matcher = Match.byteSequence lookFor : IMatcher

      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = materializerFactory.MakeTracker(reader.Position)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)
      let (consumed, remainings) = resultParts materializerFactory tracker inputSeq

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 2
      consumed |> seqEqual expectedConsumed
      remainings |> seqEqual expectedRemainings


    [<Theory>]
    [<InlineData("mno", "abcdefg")>]
    [<InlineData("p", "abcdefg")>]
    let ``with non-matching sequence should not register match`` (lookFor: string) (input: string) =
      let matcher = Match.byteSequence (utf8Bytes lookFor) : IMatcher

      let mutable reader = SequenceReader(sequenceOf input)
      let mutable tracker = MatchTracker(reader.Position, false)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.byteSequence [| 1uy |] : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable tracker = MatchTracker(reader.Position, false)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal false
      tracker.Matches |> should equal 0

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.byteSequence [| 1uy; 2uy |] : IMatcher

      let mutable reader = SequenceReader([| 2uy; 2uy |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, false)

      let startPos = reader.Position

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0
      reader.Position |> should equal startPos

  module findNext =

    [<Theory>]
    [<InlineData("de", "abcdefgh", 3)>]
    [<InlineData("ab", "abcdefgh", 0)>]
    [<InlineData("gh", "abcdefgh", 6)>]
    let ``with matching byte in sequence should register first match`` (lookFor: string) (input: string) (pos: int64) =
      let inputSeq = sequenceOf input
      let bytes = utf8Bytes lookFor

      let matcher = Match.byteSequence bytes : IMatcher

      let materializerFactory = stringMaterializer ()
      let mutable reader = SequenceReader(inputSeq)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)
      let consumed = materializeFindResult materializerFactory reader.Position length inputSeq

      result |> should equal FindResult.Found
      length |> should equal (int64 bytes.Length)
      consumed |> should equal lookFor
      reader.Consumed |> should equal pos

    [<Theory>]
    [<InlineData("ij", "abcdefgh")>]
    let ``with non-matching sequence should not register match`` (lookFor: string) (input: string) =
      let matcher = Match.byteSequence (utf8Bytes lookFor) : IMatcher

      let mutable reader = SequenceReader(sequenceOf input)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotFound

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.byteSequence [| 1uy |] : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotEnoughData

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.byteSequence [| 1uy; 2uy |] : IMatcher

      let mutable reader = SequenceReader([| 2uy; 2uy |] |> ReadOnlySequence)
      let mutable length = 0L

      let startPos = reader.Position

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotFound
      reader.Position |> should equal startPos
