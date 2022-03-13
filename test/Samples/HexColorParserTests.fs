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
open Pansar.Samples

module ``parse using hex color parser`` =

  [<Theory>]
  [<InlineData("#ffffff", 255uy, 255uy, 255uy)>]
  [<InlineData("#ffFFff", 255uy, 255uy, 255uy)>]
  [<InlineData("#FFFFFF", 255uy, 255uy, 255uy)>]
  [<InlineData("#00FF00", 0uy, 255uy, 0uy)>]
  [<InlineData("#99ff33", 153uy, 255uy, 51uy)>]
  [<InlineData("#ff6600", 255uy, 102uy, 0uy)>]
  let ``should parse color input`` hex red green blue =
    let color =
      From.string hex
      |> Parse.withException (HexColorParser.get ())

    color.Red |> should equal red
    color.Green |> should equal green
    color.Blue |> should equal blue
