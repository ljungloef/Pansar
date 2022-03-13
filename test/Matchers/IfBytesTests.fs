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
open Pansar.Buffers
open FsUnit.CustomMatchers
open System.Buffers


module ``ifBytes matcher`` =
  
  let isSpace = 
    { new IRangePredicate with

        override __.MinLength = 1
        override __.MaxLength = 1

        override __.Predicate =
          { new ISpanPredicate with
              override __.IsMatch(span, len) =
                if span[0] = 32uy then
                  len <- 1
                  true
                else
                  false} }

  module ``match`` =
  
    [<Theory>]
    [<InlineData("   abcdef", "  abcdef")>]
    [<InlineData(" abcdef", "abcdef")>]
    let ``with matching sequence should register match``
      (input: string)
      (expectedRemainings: string)
      =  
      let inputSeq = sequenceOf input

      let matcher = Match.ifRange isSpace : IMatcher

      let materializerFactory = stringMaterializer ()
      let mutable reader = SequenceReader(inputSeq)
      let mutable tracker = materializerFactory.MakeTracker(reader.Position)

      let hadEnoughData = matcher.Match(&reader, &tracker)
      let (consumed, remainings) = resultParts materializerFactory tracker inputSeq

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 1
      consumed |> should equal " "

      remainings
      |> seqEqual (utf8Bytes expectedRemainings)

    [<Theory>]
    [<InlineData("abcdef")>]
    [<InlineData("a bcdef")>]
    let ``with non-matching sequence should not register match`` (input: string) =
      let matcher = Match.ifRange isSpace : IMatcher

      let mutable reader = SequenceReader(sequenceOf input)
      let mutable tracker = MatchTracker(reader.Position, false)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.ifRange isSpace : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable tracker = MatchTracker(reader.Position, false)

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal false
      tracker.Matches |> should equal 0

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.ifRange isSpace : IMatcher

      let mutable reader = SequenceReader([| 2uy |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, false)

      let startPos = reader.Position

      let hadEnoughData = matcher.Match(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0
      reader.Position |> should equal startPos

  module matchMany =

    [<Theory>]
    [<InlineData("     abcabcabcdef", "     ", 5, "abcabcabcdef")>]
    [<InlineData("  a aaaabcdef", "  ", 2, "a aaaabcdef")>]
    [<InlineData(" aaaaabcdef", " ", 1, "aaaaabcdef")>]
    let ``with matching sequence should register match``
      (input: string)
      (expectedConsumed: string)
      (expectedMatchCount: int)
      (expectedRemainings: string)
      =
      let inputSeq = sequenceOf input

      let matcher = Match.ifRange isSpace : IMatcher

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


    [<Theory>]
    [<InlineData("abcdefg")>]
    [<InlineData("a bcdefg")>]
    let ``with non-matching sequence should not register match`` (input: string) =
      let matcher = Match.ifRange isSpace : IMatcher

      let mutable reader = SequenceReader(sequenceOf input)
      let mutable tracker = MatchTracker(reader.Position, false)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.ifRange isSpace : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable tracker = MatchTracker(reader.Position, false)

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal false
      tracker.Matches |> should equal 0

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.ifRange isSpace : IMatcher

      let mutable reader = SequenceReader([| 2uy; 2uy |] |> ReadOnlySequence)
      let mutable tracker = MatchTracker(reader.Position, false)

      let startPos = reader.Position

      let hadEnoughData = matcher.MatchMany(&reader, &tracker)

      hadEnoughData |> should equal true
      tracker.Matches |> should equal 0
      reader.Position |> should equal startPos

  module findNext =

    [<Theory>]
    [<InlineData("abcde fgh", 5)>]
    [<InlineData(" abcdefgh", 0)>]
    [<InlineData("abcdefgh ", 8)>]
    let ``with match from predicate should register first match`` (input: string) (pos: int64) =
      let inputSeq = sequenceOf input

      let matcher = Match.ifRange isSpace : IMatcher

      let materializerFactory = stringMaterializer ()
      let mutable reader = SequenceReader(inputSeq)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)
      let consumed = materializeFindResult materializerFactory reader.Position length inputSeq

      result |> should equal FindResult.Found
      length |> should equal 1L
      consumed |> should equal " "
      reader.Consumed |> should equal pos

    [<Theory>]
    [<InlineData("abcdefgh")>]
    let ``with non-matching sequence should not register match`` (input: string) =
      let matcher = Match.ifRange isSpace : IMatcher

      let mutable reader = SequenceReader(sequenceOf input)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotFound

    [<Fact>]
    let ``with not enough remaining items should return false`` () =
      let matcher = Match.ifRange isSpace : IMatcher

      let mutable reader = SequenceReader(ReadOnlySequence.Empty)
      let mutable length = 0L

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotEnoughData

      reader.Position
      |> should equal reader.Sequence.Start

    [<Fact>]
    let ``with unmatch should not advance reader position`` () =
      let matcher = Match.ifRange isSpace : IMatcher

      let mutable reader = SequenceReader([| 2uy; 2uy |] |> ReadOnlySequence)
      let mutable length = 0L

      let startPos = reader.Position

      let result = matcher.FindNext(&reader, &length)

      result |> should equal FindResult.NotFound
      reader.Position |> should equal startPos
