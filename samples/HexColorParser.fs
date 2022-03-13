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

namespace Pansar.Samples

open Pansar
open Pansar.Text
open Pansar.Text.Parsers

module HexColorParser =

  type Color = { Red: byte; Green: byte; Blue: byte }

  let get () =

    let numberSign = Match.char '#' /> Materialize.ignore

    let channel = leftSlice 2 (utf8Bytef NumberFormat.Hexadecimal)

    numberSign .> consume3 channel
    |>> (fun ((r, g), b) -> { Red = r; Green = g; Blue = b })
