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
open Pansar.Text
open Pansar.Text.Predicates.Ascii
open FsUnit.CustomMatchers

module ApplyingCombinatorsTests =

  module whileConsuming =

    [<Theory>]
    [<InlineData("abcdef_", "abcdef")>]
    [<InlineData("abc|defghij", "abc")>]
    [<InlineData("a^c|defghij", "a")>]
    let ``should parse as expected with consumer`` input expected =
      let sequence = sequenceOf input

      let parser = Match.if' isAlphaNumeric /> Materialize.toString

      let actual =
        sequence
        |> Parse.withException (whileConsuming parser)

      actual |> should equal expected

    [<Fact>]
    let ``should propogate consumer failure`` () =
      let sequence = sequenceOf "^abc"

      let parser = Match.if' isAlphaNumeric /> Materialize.toString

      let actual =
        sequence
        |> Parse.eval (whileConsuming1 parser)
        |> shouldBeFailed

      actual |> should be (ofCase <@ Unmatch @>)

    [<Fact>]
    let ``should propogate eof error failure from consumer`` () =
      let sequence = sequenceOf ""

      let parser = Match.if' isAlphaNumeric /> Materialize.toString

      let actual =
        sequence
        |> Parse.eval (whileConsuming1 parser)
        |> shouldBeFailed

      actual
      |> should be (ofCase <@ UnexpectedEndOfSeq @>)


    [<Fact>]
    let ``should parse as expected with parser`` () =
      let sequence = sequenceOf "ab cd ef"

      let delimiter = Match.char ' ' /> Materialize.toString
      let content = Parsers.utf8String ()

      let actual =
        sequence
        |> Parse.withException (whileConsuming (leftSliceTill delimiter content))

      actual |> seqEqual [ "ab"; "cd" ]

    [<Fact>]
    let ``should propogate parser failure`` () =
      let sequence = sequenceOf "ab cd ef"

      let parser = Parsers.failure <| Unknown "Error"

      let failureCode =
        sequence
        |> Parse.eval (whileConsuming1 parser)
        |> shouldBeFailed

      failureCode |> should be (ofCase <@ Unknown @>)

    [<Fact>]
    let ``should succeed a failed parser if min is zero`` () =
      let sequence = sequenceOf "ab cd ef"

      let parser = Parsers.failure <| Unknown "Error"

      let result =
        sequence
        |> Parse.withException (whileConsuming parser)

      result |> should haveLength 0

  module choose =

    [<Theory>]
    [<InlineData("first", "a")>]
    [<InlineData("second", "b")>]
    [<InlineData("third", "c")>]
    [<InlineData("fourth", "d")>]
    [<InlineData("fifth", "e")>]
    let ``choose should parse from correct branch`` input expected =
      let sequence = sequenceOf input

      let parser =
        choose [ (Match.string "first" /> Materialize.toString, Parsers.get "a")
                 (Match.string "second" /> Materialize.toString, Parsers.get "b")
                 (Match.string "third" /> Materialize.toString, Parsers.get "c")
                 (Match.string "fourth" /> Materialize.toString, Parsers.get "d")
                 (Match.string "fifth" /> Materialize.toString, Parsers.get "e") ]

      let actual =
        sequence
        |> Parse.eval parser
        |> materialize sequence

      actual |> should equal expected

    [<Fact>]
    let ``choose with no match should fail with all failCodes`` () =
      let sequence = sequenceOf "a"

      let parser =
        choose [ (Match.string "b" /> Materialize.toString, Parsers.get "b")
                 (Match.string "c" /> Materialize.toString, Parsers.get "c") ]

      let actual = sequence |> Parse.eval parser |> shouldBeFailed

      actual |> should be (ofCase <@ Aggregated @>)
