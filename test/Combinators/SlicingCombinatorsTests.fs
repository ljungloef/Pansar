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

module ``slicing combinators`` =

  module leftSlice =

    [<Theory>]
    [<InlineData("abcdefghijkl", 5, "abcde", "fghijkl")>]
    [<InlineData("abcdefghijkl", 0, "abcdefghijkl")>]
    [<InlineData("abcdefghijkl", 12, "abcdefghijkl", "")>]
    let ``leftSlice should apply parser on a slice in the given size`` input length expected expectedRemainings =
      let sequence = sequenceOf input

      let str = Parsers.utf8String ()
      let parser = leftSlice length str

      let result = sequence |> Parse.eval parser

      let actual = result |> materialize sequence
      let remainings = result |> remainings sequence |> sequenceToString

      actual |> should equal expected
      remainings |> should equal expectedRemainings
 
    [<Fact>]
    let ``leftSlice should position the reader at the end of the slice`` () =
      let parser = leftSlice 2 (Parsers.utf8String ())

      let (first, second) =
        From.string "abcd"
        |> Parse.withException (parser <.> parser)

      first |> should equal "ab"
      second |> should equal "cd"

    [<Fact>]
    let ``leftSlice with size parameter outside of available content should return unexpected end of seq`` () =
      let sequence = sequenceOf "abc"

      let str = Parsers.utf8String ()
      let parser = leftSlice (int sequence.Length + 1) str

      let result = sequence |> Parse.eval parser

      result |> shouldBeEndOfSequence

  module leftSliceTill =

    [<Theory>]
    [<InlineData("abcdefgh\r\nijklmnop", "abcdefgh", "ijklmnop", "\r\n")>]
    [<InlineData("abc", "a", "c", "b")>]
    let ``leftSliceTill should parse as expected`` input expected expectedRemainings (sep: string) =
      let sequence = sequenceOf input

      let sep = Match.charSequence (sep.ToCharArray()) /> Materialize.toString
      let content = Parsers.utf8String ()

      let result = sequence |> Parse.eval (leftSliceTill sep content)

      let actual = result |> materialize sequence
      let remaindings = result |> remainings sequence |> sequenceToString

      actual |> should equal expected
      remaindings |> should equal expectedRemainings

    [<Fact>]
    let ``leftSliceTill with include sep should include sep in slice`` () =
      let sequence = sequenceOf "abc def"

      let sep = Match.char ' ' /> Materialize.toString
      let content = Parsers.utf8String ()

      let result =
        sequence
        |> Parse.eval (leftSliceTillOpts sep content { LeftSliceTillOptions.Default with IncludeSepInSlice = true })

      let actual = result |> materialize sequence
      let remaindings = result |> remainings sequence |> sequenceToString

      actual |> should equal "abc "
      remaindings |> should equal "def"

    [<Theory>]
    [<InlineData("", "a")>]
    [<InlineData("abc", "bced")>]
    let ``leftSliceTill with not enough data should fail with endOfSequence`` input (sep: string) =
      let sequence = sequenceOf input

      let sep = Match.charSequence (sep.ToCharArray()) /> Materialize.toString
      let content = Parsers.utf8String ()

      let actual =
        sequence
        |> Parse.eval (leftSliceTill sep content)
        |> shouldBeFailed

      actual
      |> should be (ofCase <@ UnexpectedEndOfSeq @>)

  module sliceBy =

    [<Theory>]
    [<InlineData("abcdefgh\r\nijklmnop\r\nqrstuv\r\nxyz", "", "\r\n", Skip = "includeLast disabled atm")>]
    [<InlineData("a\r\nb", "", "\r\n", Skip = "includeLast disabled atm")>]
    let ``manyBy newLine with includeLast should slice out as expected`` input expectedRemainings (sep: string) =
      let sequence = sequenceOf input
      let expected = input.Split(sep) |> List.ofArray

      let sep = Match.charSequence (sep.ToCharArray()) /> Materialize.toString
      let content = Match.if' isAlphaNumeric /> Materialize.toString

      let result = sequence |> Parse.eval (sliceBy sep content)

      let actual = result |> materialize sequence
      let remaindings = result |> remainings sequence |> sequenceToString

      actual |> should equal expected
      remaindings |> should equal expectedRemainings

    [<Fact>]
    let ``should propogate consumer failure`` () =
      let sequence = sequenceOf "1\r\nabc"

      let sep = Match.string "\r\n" /> Materialize.toString
      let content = Match.if' isLetter /> Materialize.toString

      let failureCode =
        sequence
        |> Parse.eval (sliceBy sep content)
        |> shouldBeFailed

      failureCode |> should be (ofCase <@ Unmatch @>)

    [<Fact>]
    let ``should propogate parser failure`` () =
      let sequence = sequenceOf "abc\r\ndef"

      let sep = Match.string "\r\n" /> Materialize.toString
      let content = Parsers.failure (Unknown "")

      let failureCode =
        sequence
        |> Parse.eval (sliceBy sep content)
        |> shouldBeFailed

      failureCode |> should be (ofCase <@ Unknown @>)

    [<Theory>]
    [<InlineData("abcdefgh\r\nijklmnop\r\nqrstuv\r\nxyz", "xyz", "\r\n")>]
    [<InlineData("abcdefgh\r\n\r\nqrstuv\r\nxyz", "xyz", "\r\n")>]
    [<InlineData("a\r\nb", "b", "\r\n")>]
    let ``should slice out and apply parsers as expected`` input expectedRemainings (sep: string) =
      let sequence = sequenceOf input
      let parts = input.Split(sep)

      let expected = parts |> Array.take (parts.Length - 1)

      let sep = Match.charSequence (sep.ToCharArray()) /> Materialize.toString
      let content = Parsers.utf8String ()

      let result = sequence |> Parse.eval (sliceBy sep content)

      let actual = result |> materialize sequence
      let remainings = result |> remainings sequence |> sequenceToString

      actual |> should equal expected
      remainings |> should equal expectedRemainings

    [<Theory>]
    [<InlineData("abcdefgh\r\nijklmnop\r\nqrstuv\r\nxyz", "ijklmnop\r\nqrstuv\r\nxyz", "\r\n", 1, 1)>]
    [<InlineData("abcdefgh\r\nijklmnop\r\nqrstuv\r\nxyz", "qrstuv\r\nxyz", "\r\n", 2, 2)>]
    [<InlineData("abcdefgh\r\nijklmnop\r\nqrstuv\r\nxyz", "xyz", "\r\n", 3, 3)>]
    [<InlineData("abcdefgh\r\nijklmnop\r\nqrstuv\r\nxyz", "xyz", "\r\n", 4, 3)>]
    [<InlineData("abcdefgh\r\nijklmnop\r\nqrstuv\r\nxyz", "xyz", "\r\n", 5, 3)>]
    let ``should slice out and apply parsers as expected when limit is set``
      input
      expectedRemainings
      (sep: string)
      limit
      expectedCount
      =
      let sequence = sequenceOf input

      let expected = input.Split(sep) |> Array.take expectedCount

      let sep = Match.charSequence (sep.ToCharArray()) /> Materialize.toString
      let content = Parsers.utf8String ()

      let result =
        sequence
        |> Parse.eval (sliceByOpts sep content ({ SliceByOptions.Default with Limit = Some limit }))

      let actual = result |> materialize sequence

      let remaindings = result |> remainings sequence |> sequenceToString

      actual |> should equal expected
      remaindings |> should equal expectedRemainings

  module slice2 =

    [<Fact>]
    let ``should apply parsers in sequence`` () =
      let sequence = sequenceOf "abc def"

      let sep = Match.if' isWhitespace /> Materialize.toString
      let content = Parsers.utf8String ()

      let (a, b) =
        sequence
        |> Parse.withException (slice2 sep (content, content))

      a |> should equal "abc"
      b |> should equal "def"

    [<Fact>]
    let ``should fail with unmatch when sequence does not contain enough seperators`` () =
      let sequence = sequenceOf "abcdef"

      let sep = Match.if' isWhitespace /> Materialize.toString
      let content = Parsers.utf8String ()

      let failure =
        sequence
        |> Parse.eval (slice2 sep (content, content))
        |> shouldBeFailed

      failure |> should be (ofCase <@ Unmatch @>)

  module slice3 =

    [<Fact>]
    let ``should apply parsers in sequence`` () =
      let sequence = sequenceOf "abc def ghi"

      let sep = Match.if' isWhitespace /> Materialize.toString
      let content = Parsers.utf8String ()

      let (a, b, c) =
        sequence
        |> Parse.withException (slice3 sep (content, content, content))

      a |> should equal "abc"
      b |> should equal "def"
      c |> should equal "ghi"


    [<Theory>]
    [<InlineData("abc defghi")>]
    [<InlineData("abcdefghi")>]
    let ``should fail with unmatch when sequence does not contain enough seperators`` input =
      let sequence = sequenceOf input

      let sep = Match.if' isWhitespace /> Materialize.toString
      let content = Parsers.utf8String ()

      let failure =
        sequence
        |> Parse.eval (slice3 sep (content, content, content))
        |> shouldBeFailed

      failure |> should be (ofCase <@ Unmatch @>)

  module slice4 =

    [<Fact>]
    let ``should apply parsers in sequence`` () =
      let sequence = sequenceOf "abc def ghi jkl"

      let sep = Match.if' isWhitespace /> Materialize.toString
      let content = Parsers.utf8String ()

      let (a, b, c, d) =
        sequence
        |> Parse.withException (slice4 sep (content, content, content, content))

      a |> should equal "abc"
      b |> should equal "def"
      c |> should equal "ghi"
      d |> should equal "jkl"


    [<Theory>]
    [<InlineData("abc def ghijkl")>]
    [<InlineData("abc defghijkl")>]
    [<InlineData("abcdefghijkl")>]
    let ``should fail with unmatch when sequence does not contain enough seperators`` input =
      let sequence = sequenceOf input

      let sep = Match.if' isWhitespace /> Materialize.toString
      let content = Parsers.utf8String ()

      let failure =
        sequence
        |> Parse.eval (slice4 sep (content, content, content, content))
        |> shouldBeFailed

      failure |> should be (ofCase <@ Unmatch @>)

  module slice5 =

    [<Fact>]
    let ``should apply parsers in sequence`` () =
      let sequence = sequenceOf "abc def ghi jkl mno"

      let sep = Match.if' isWhitespace /> Materialize.toString
      let content = Parsers.utf8String ()

      let (a, b, c, d, e) =
        sequence
        |> Parse.withException (slice5 sep (content, content, content, content, content))

      a |> should equal "abc"
      b |> should equal "def"
      c |> should equal "ghi"
      d |> should equal "jkl"
      e |> should equal "mno"


    [<Theory>]
    [<InlineData("abc def ghi jklmno")>]
    [<InlineData("abc def ghijklmno")>]
    [<InlineData("abc defghijklmno")>]
    [<InlineData("abcdefghijklmno")>]
    let ``should fail with unmatch when sequence does not contain enough seperators`` input =
      let sequence = sequenceOf input

      let sep = Match.if' isWhitespace /> Materialize.toString
      let content = Parsers.utf8String ()

      let failure =
        sequence
        |> Parse.eval (slice5 sep (content, content, content, content, content))
        |> shouldBeFailed

      failure |> should be (ofCase <@ Unmatch @>)

  module sliceSome =

    open SliceSomeDefinition

    [<Fact>]
    let ``should slice as expected`` () =
      let sequence = sequenceOf "abc 22 ghi 33 mno"

      let str = Parsers.utf8String ()
      let int = Parsers.utf8Int32 ()
      let space = Match.char ' ' /> Materialize.toString

      let slicer =
        define (space, str) 
        |= (space, int) 
        |. space
        |= (space, int)
        |> rest str
        |> sliceSome

      let (((a, b), c), d) = sequence |> Parse.withException slicer

      a |> should equal "abc"
      b |> should equal 22
      c |> should equal 33
      d |> should equal "mno"

    [<Fact>]
    let ``should slice as expected when containing empty entries`` () =
      let sequence = sequenceOf "abc;;22;ghi;33;mno"

      let str = Parsers.utf8String ()
      let delimiter = Match.char ';' /> Materialize.toString

      let slicer =
        define (delimiter, str) 
        |= (delimiter, str) 
        |. delimiter
        |= (delimiter, str)
        |. delimiter
        |> rest str
        |> sliceSome

      let (((a, b), c), d) = sequence |> Parse.withException slicer

      a |> should equal "abc"
      b |> should equal ""
      c |> should equal "ghi"
      d |> should equal "mno"

    [<Fact>]
    let ``should skip start when created using defineExcl`` () =
      let sequence = sequenceOf "abc;cde;fgh"

      let str = Parsers.utf8String ()
      let delimiter = Match.char ';' /> Materialize.toString

      let slicer =
        defineExcl delimiter |= (delimiter, str)
        |> rest str
        |> sliceSome

      let ((a, b), c) = sequence |> Parse.withException slicer

      a |> should equal Void
      b |> should equal "cde"
      c |> should equal "fgh"

    [<Fact>]
    let ``should fail when parser applied to slice fails`` () =
      let sequence = sequenceOf "abc;cde;fgh"

      let str = Parsers.utf8String ()
      let fail = Parsers.failure Unmatch
      let delimiter = Match.char ';' /> Materialize.toString

      let slicer =
        define (delimiter, str) |= (delimiter, fail)
        |> rest str
        |> sliceSome

      let failure = sequence |> Parse.eval slicer |> shouldBeFailed

      failure |> should be (ofCase <@ Unmatch @>)

    [<Fact>]
    let ``should fail when delimeter missing`` () =
      let sequence = sequenceOf "abc;cdefgh"

      let str = Parsers.utf8String ()
      let delimiter = Match.char ';' /> Materialize.toString

      let slicer =
        define (delimiter, str) 
        |= (delimiter, str)
        |> rest str
        |> sliceSome

      let failure = sequence |> Parse.eval slicer |> shouldBeFailed

      failure |> should be (ofCase <@ Unmatch @>)
