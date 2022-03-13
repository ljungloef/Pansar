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
open FsUnit.CustomMatchers
open Xunit.Sdk

module ``parser combinators`` =

  module andThen =

    [<Fact>]
    let ``parse ab andKeepBoth parse cd should return ab cd from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'c' .. 'd' ]
        /> Materialize.toString

      let combined = first <.> second

      let (a, b) = sequence |> Parse.withException combined

      a |> should equal "ab"
      b |> should equal "cd"

    [<Fact>]
    let ``parse ab andKeepLeft parse cd should return ab from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'c' .. 'd' ]
        /> Materialize.toString

      let combined = first <. second

      let result = sequence |> Parse.withException combined

      result |> should equal "ab"

    [<Fact>]
    let ``parse ab andKeepRight parse cd should return cd from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'c' .. 'd' ]
        /> Materialize.toString

      let combined = first .> second

      let result = sequence |> Parse.withException combined

      result |> should equal "cd"

    [<Fact>]
    let ``parse ab andKeepNone parse cd should return Void from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'c' .. 'd' ]
        /> Materialize.toString

      let combined = first >.< second

      let result = sequence |> Parse.withException combined

      result |> should be (ofCase <@ Void @>)

  module orElse =

    [<Fact>]
    let ``parse ab orKeepEither parse cd should return Choice 1of2 ab from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'c' .. 'd' ]
        /> Materialize.toString

      let combined = first <^> second

      let actual = sequence |> Parse.withException combined

      match actual with
      | Choice1Of2 result -> result |> should equal "ab"
      | Choice2Of2 _ -> raise (XunitException("Should take left.."))

    [<Fact>]
    let ``parse xy orKeepEither parse ab should return Choice 2of2 ab from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'x' .. 'y' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let combined = first <^> second

      let actual = sequence |> Parse.withException combined

      match actual with
      | Choice1Of2 _ -> raise (XunitException("Should take right.."))
      | Choice2Of2 result -> result |> should equal "ab"

    [<Fact>]
    let ``parse ac orKeepEither parse ab should return Choice 2of2 ab from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      // Important to test 'a' here, because 'a' will match once and then fail, causing the reader to advance once
      // The orElse parser should reset the position before running the second parser
      let first =
        Match.charSequence [ 'a'; 'c' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let combined = first <^> second

      let actual = sequence |> Parse.withException combined

      match actual with
      | Choice1Of2 bad -> raise (XunitException($"Should take right.. '%A{bad}'"))
      | Choice2Of2 result -> result |> should equal "ab"

    [<Fact>]
    let ``parse xy orKeepEither parse mn should return Failed from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'x' .. 'y' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'm' .. 'n' ]
        /> Materialize.toString

      let combined = first <^> second

      let actual = sequence |> Parse.eval combined

      actual |> shouldBeFailed

    [<Fact>]
    let ``parse ab orKeepLeft parse cd should return Some ab from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'c' .. 'd' ]
        /> Materialize.toString

      let combined = first <^ second

      let actual = sequence |> Parse.withException combined

      match actual with
      | Some result -> result |> should equal "ab"
      | None -> raise (XunitException("Should take left.."))

    [<Fact>]
    let ``parse xy orKeepLeft parse ab should return None from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'x' .. 'y' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let combined = first <^ second

      let actual = sequence |> Parse.withException combined

      match actual with
      | Some _ -> raise (XunitException("Should take right.."))
      | None -> ()

    [<Fact>]
    let ``parse xy orKeepLeft parse mn should return Failure from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'x' .. 'y' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'm' .. 'n' ]
        /> Materialize.toString

      let combined = first <^ second

      let actual = sequence |> Parse.eval combined

      actual |> shouldBeFailed

    [<Fact>]
    let ``parse ab orKeepRight parse cd should return None from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'c' .. 'd' ]
        /> Materialize.toString

      let combined = first ^> second

      let actual = sequence |> Parse.withException combined

      match actual with
      | Some _ -> raise (XunitException("Should take right.."))
      | None -> ()

    [<Fact>]
    let ``parse xy orKeepRight parse ab should return Some ab from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'x' .. 'y' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let combined = first ^> second

      let actual = sequence |> Parse.withException combined

      match actual with
      | Some result -> result |> should equal "ab"
      | None -> raise (XunitException("Should take right.."))

    [<Fact>]
    let ``parse xy orKeepRight parse mn should return Failure from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'x' .. 'y' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'm' .. 'n' ]
        /> Materialize.toString

      let combined = first ^> second

      let actual = sequence |> Parse.eval combined

      actual |> shouldBeFailed


    [<Fact>]
    let ``parse ab orKeepNone parse cd should return Void from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'c' .. 'd' ]
        /> Materialize.toString

      let combined = first ^^ second

      let actual = sequence |> Parse.asResult combined

      match actual with
      | Ok _ -> ()
      | Error _ -> raise (XunitException("Should not fail.."))

    [<Fact>]
    let ``parse xy orKeepNone parse ab should return Void from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'x' .. 'y' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'a' .. 'b' ]
        /> Materialize.toString

      let combined = first ^^ second

      let actual = sequence |> Parse.eval combined

      match actual with
      | Consume _ -> ()
      | Failure _ -> raise (XunitException("Should not fail.."))

    [<Fact>]
    let ``parse xy orKeepNone parse mn should return Failure from abcdef`` () =
      let sequence = sequenceOf "abcdef"

      let first =
        Match.charSequence [ 'x' .. 'y' ]
        /> Materialize.toString

      let second =
        Match.charSequence [ 'm' .. 'n' ]
        /> Materialize.toString

      let combined = first ^^ second

      let actual = sequence |> Parse.eval combined

      actual |> shouldBeFailed
