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
open Pansar.Binary
open FsUnit.CustomMatchers
open System
open System.Buffers

module ``binary int16Le parser and matcher`` =

  [<Theory>]
  [<InlineData(10s)>]
  [<InlineData(-120s)>]
  [<InlineData(0s)>]
  let ``int16Le matcher should parse and match supplied value`` (expected: int16) =
    let sequence =
      BitConverter.GetBytes(expected)
      |> ReadOnlySequence

    let actual =
      sequence
      |> Parse.withException (Match.int16Le expected /> Materialize.toInt16Le)

    actual |> should equal expected

  [<Theory>]
  [<InlineData("8A-27-16-00", 10_122s)>]
  [<InlineData("8A-27", 10_122s)>]
  [<InlineData("16-00", 22s)>]
  let ``int16Le parser should be able to parse int16 value from binary sequence`` (hexSequence: string) expected =
    let sequence = sequenceFromHexString hexSequence

    let actual =
      sequence
      |> Parse.withException (Parsers.int16Le ())

    actual |> should equal expected

  [<Fact>]
  let ``matching int16le with another int16 should return Unmatch`` =
    let sequence = sequenceFromHexString "8A-27"

    let parser = Match.int16Le 22s /> Materialize.toInt16Le

    let actual = sequence |> Parse.eval parser |> shouldBeFailed

    actual |> should be (ofCase <@ Unmatch @>)

  [<Fact>]
  let ``int16Le with not enough data should return EndOfSequence`` =
    let sequence = sequenceFromHexString "16"

    let pos =
      sequence
      |> Parse.eval (Parsers.int16Le ())
      |> shouldBeEndOfSequence

    pos |> should equal sequence.End
