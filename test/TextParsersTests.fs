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
open System

module Something =

  [<Theory>]
  [<InlineData("   many-spaces", ' ', "many-spaces")>]
  [<InlineData(" single-space", ' ', "single-space")>]
  [<InlineData("   ", ' ', "")>]
  let ``utf8StringTrimStart should remove preceeding chars`` input trimChar expected =
    let sequence = sequenceOf input

    let actual =
      sequence
      |> Parse.withException (Parsers.utf8StringTrimStart trimChar)

    actual |> should equal expected
