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

module bind =

  [<Fact>]
  let ``parse a bound to b singleton should return b from abc`` () =
    let parser = Match.char 'a' /> Materialize.toString
    let combined = parser >>= fun _ -> Parsers.utf8Rune ()

    let result =
      From.string "abc"
      |> Parse.withException combined

    result.ToString() |> should equal "b"

module control =

  [<Theory>]
  [<InlineData("abcdefghijkl", 5L, "abcde", "fghijkl")>]
  [<InlineData("abcdefghijkl", 1L, "a", "bcdefghijkl")>]
  [<InlineData("abcdefghijkl", 12L, "abcdefghijkl", "")>]
  let ``leftSlice should only apply parser to sliced out sequence`` input offset expected expectedRemainings =
    let sequence = sequenceOf input

    let parser = leftSlice offset (Parsers.utf8String ())

    let result = sequence |> Parse.eval parser

    let actual = result |> materialize sequence
    let actualRemainings = result |> remainings sequence |> sequenceToString

    actual |> should equal expected

    actualRemainings
    |> should equal expectedRemainings

module retag =

  [<Fact>]
  let ``retag should set a new tag to parser`` () =
    let expected = ParserTag "new-tag"

    let sut = Parsers.utf8String ()
    let retaggedParser = sut <?> expected

    retaggedParser.Tag |> should equal expected
