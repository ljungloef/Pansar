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
open Pansar
open Pansar.Text
open System

module Utf8ValueParserTests =

  open Microsoft.FSharp.Core.LanguagePrimitives
  

  module utf8Int32Tests =

    [<Theory>]
    [<InlineData("1234öööööööö", NumberFormat.Decimal, 1234, "öööööööö")>]
    [<InlineData("1234öööööööö", NumberFormat.Decimal, 1234, "öööööööö")>]
    [<InlineData("-1234öööööööö", NumberFormat.Decimal, -1234, "öööööööö")>]
    [<InlineData("1234.1öööööööö", NumberFormat.Decimal, 1234, ".1öööööööö")>]
    [<InlineData("0001öööööööö", NumberFormat.Hexadecimal, 1, "öööööööö")>]
    [<InlineData("006Eöööööööö", NumberFormat.Hexadecimal, 110, "öööööööö")>]
    let ``should parse as expected`` input format expectedResult expectedRemainings  =
      let sequence = sequenceOf input

      let (value, remainings) =
        sequence
        |> Parse.eval (Parsers.utf8Int32f format)
        |> splitResult sequence

      value |> should equal expectedResult
      remainings |> sequenceToString |> should equal expectedRemainings

    [<Fact>]
    let ``should fail with unmatch when value is larger than allowed by type`` ()  =
      let failure =
        From.string $"{Int32.MaxValue}1"
        |> Parse.eval (Parsers.utf8Int32 ())
        |> shouldBeFailed

      failure
      |> should be (ofCase <@ Unmatch @>)

    [<Fact>]
    let ``should fail as expected`` ()  =
      let failure =
        From.string "abcdef"
        |> Parse.eval (Parsers.utf8Int32 ())
        |> shouldBeFailed

      failure
      |> should be (ofCase <@ Unmatch @>)

    [<Fact>]
    let ``should throw if unsupported format is used`` ()  =
      let act () = Parsers.utf8Int32f (EnumOfValue<Char, NumberFormat>('y')) |> ignore

      act |> should throw typeof<FormatException>

    [<Fact>]
    let ``with not enough data should return EndOfSequence`` =
      let sequence = sequenceOf ""

      let actual =
        sequence
        |> Parse.eval (Parsers.utf8Int32 ())
        |> shouldBeFailed

      actual
      |> should be (ofCase <@ UnexpectedEndOfSeq @>)
