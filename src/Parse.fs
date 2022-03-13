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

namespace Pansar

open System.Buffers
open System.Text

[<RequireQualifiedAccess>]
module From =

  // Parse a binary array
  let inline byteArray (buffer: byte array) = buffer |> ReadOnlySequence

  /// Use the content of a string as parsing source
  let inline string (s: string) =
    s |> Encoding.UTF8.GetBytes |> ReadOnlySequence

  /// Use a sequence as parsing source
  let inline sequence (seq: ReadOnlySequence<byte>) = seq

[<RequireQualifiedAccess>]
module Parse =

  open ParserUtils

  /// Run the parser on the sequence but without materializing any values
  let inline eval parser (seq: ReadOnlySequence<byte>) =
    let mutable ctx = ParseContext(SequenceReader<byte>(seq), true)

    match parse &ctx parser with
    | true, mat -> Consume(mat, ctx.Reader.Position)
    | false, _ -> Failure(ctx.Error.Tag, ctx.Error.Code)

  /// Run the parser on the sequence. If the parser succeeds, the value is materialized and returned. If the parser fails, a `ParseException` is raised.
  let inline withException parser (seq: ReadOnlySequence<byte>) =
    let mutable ctx = ParseContext(SequenceReader<byte>(seq), true)

    match parse &ctx parser with
    | true, mat -> MaterializerFunc.run mat &seq
    | false, _ -> raise (ParseException(ctx.Error.Tag, ctx.Error.Code))

  /// Run the parser on the sequence. If the parser succeeds, the value is materialized and returned in the `Ok` case. If the parser fails, the failing
  /// parser's tag, and the corresponding `ParseFailureCode`, is included in the `Error` case of the `Result`
  let inline asResult parser (seq: ReadOnlySequence<byte>) =
    let mutable ctx = ParseContext(SequenceReader<byte>(seq), true)

    match parse &ctx parser with
    | true, mat -> Ok(MaterializerFunc.run mat &seq)
    | false, _ -> Error(ctx.Error.Tag, ctx.Error.Code)
