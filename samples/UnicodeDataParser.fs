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
open Pansar.Binary
open System.Net.Http
open System.Text

module UnicodeDataParser =

  open SliceSomeDefinition

  type UnicodeData =
    { Code: int
      Char: Rune option
      Name: string
      GeneralCategory: string }

  let crlf = Match.char '\n'
  let delimiter = Match.char ';'

  let str = utf8String ()
  let hex = utf8Int32f NumberFormat.Hexadecimal

  let row =
    define (delimiter, hex)
    |= (delimiter, str)
    |= (delimiter, str)
    |> sliceSome
    |>> (fun ((code, name), category) ->
      { Code = code
        Char =
          try
            Rune(code) |> Some
          with
          | _ -> None
        Name = name
        GeneralCategory = category })

  let parser = sliceBy crlf row
