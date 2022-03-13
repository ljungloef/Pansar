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
open FsUnit.CustomMatchers
open Pansar.Text
open System.Text
open System

module ``text predicates`` =

  let fixture stop =
    seq {
      for i in 0x00..stop do
        let rune = Rune(i)

        let arr = Array.zeroCreate rune.Utf8SequenceLength
        let _ = rune.EncodeToUtf8((arr.AsSpan()))

        (rune, arr)
    }

  module ascii =

    open Predicates.Ascii

    let blockEnd = 0x7F

    [<Fact>]
    let ``can identify all whitespaces`` () =
      for (rune, utf8) in fixture blockEnd do
        assert (utf8.Length = 1)

        let expected = Rune.IsWhiteSpace(rune)
        let actual = isWhitespace utf8[0]

        actual |> should equal expected

    [<Fact>]
    let ``can identify all lowerCase`` () =
      for (rune, utf8) in fixture blockEnd do
        assert (utf8.Length = 1)

        let expected = Rune.IsLower(rune)
        let actual = isLowerCase utf8[0]

        actual |> should equal expected

    [<Fact>]
    let ``can identify all upperCase`` () =
      for (rune, utf8) in fixture blockEnd do
        assert (utf8.Length = 1)

        let expected = Rune.IsUpper(rune)
        let actual = isUpperCase utf8[0]

        actual |> should equal expected

    [<Fact>]
    let ``can identify all letters`` () =
      for (rune, utf8) in fixture blockEnd do
        assert (utf8.Length = 1)

        let expected = Rune.IsLetter(rune)
        let actual = isLetter utf8[0]

        actual |> should equal expected

    [<Fact>]
    let ``can identify all alphaNumeric`` () =
      for (rune, utf8) in fixture blockEnd do
        assert (utf8.Length = 1)

        let expected = Rune.IsLetterOrDigit(rune)
        let actual = isAlphaNumeric utf8[0]

        actual |> should equal expected

    [<Fact>]
    let ``can identify all numeric`` () =
      for (rune, utf8) in fixture blockEnd do
        assert (utf8.Length = 1)

        let expected = Rune.IsDigit(rune)
        let actual = isNumeric utf8[0]

        actual |> should equal expected

  module latin1 =

    open Predicates.Utf8

    let blockEnd = 0xFF

    [<Fact>]
    let ``can identify all whitespaces`` () =
      let mutable len = Unchecked.defaultof<int64>

      for (rune, utf8) in fixture blockEnd do
        let expected = Rune.IsWhiteSpace(rune)
        let sut = isWhitespace ()
        let actual = sut.Predicate.IsMatch(utf8, &len)

        (actual, rune.Value)
        |> should equal (expected, rune.Value)

    [<Fact>]
    let ``can identify all lowerCase`` () =
      let mutable len = Unchecked.defaultof<int64>

      for (rune, utf8) in fixture blockEnd do
        let expected = Rune.IsLower(rune)
        let sut = isLowerCase ()
        let actual = sut.Predicate.IsMatch(utf8, &len)

        actual |> should equal expected

    [<Fact>]
    let ``can identify all upperCase`` () =
      let mutable len = Unchecked.defaultof<int64>

      for (rune, utf8) in fixture blockEnd do
        let expected = Rune.IsUpper(rune)
        let sut = isUpperCase ()
        let actual = sut.Predicate.IsMatch(utf8, &len)

        actual |> should equal expected

    [<Fact>]
    let ``can identify all letters`` () =
      let mutable len = Unchecked.defaultof<int64>

      for (rune, utf8) in fixture blockEnd do
        let expected = Rune.IsLetter(rune)
        let sut = isLetter ()
        let actual = sut.Predicate.IsMatch(utf8, &len)

        actual |> should equal expected

    [<Fact>]
    let ``can identify all alphaNumeric`` () =
      let mutable len = Unchecked.defaultof<int64>

      for (rune, utf8) in fixture blockEnd do
        let expected = Rune.IsLetterOrDigit(rune)
        let sut = isAlphaNumeric ()
        let actual = sut.Predicate.IsMatch(utf8, &len)

        actual |> should equal expected

    [<Fact>]
    let ``can identify all numeric`` () =
      let mutable len = Unchecked.defaultof<int64>

      for (rune, utf8) in fixture blockEnd do
        let expected = Rune.IsNumber(rune)
        let sut = isNumeric ()
        let actual = sut.Predicate.IsMatch(utf8, &len)

        actual |> should equal expected
