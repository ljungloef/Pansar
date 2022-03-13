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

namespace Pansar.Binary

open Pansar
open System.Buffers
open System
open System.Buffers.Binary
open MaterializerFactory

[<RequireQualifiedAccess>]
module Match =

  module Helpers =

    let private writeInt16le (value: int16) =
      let buffer = Array.zeroCreate 2
      BinaryPrimitives.WriteInt16LittleEndian(buffer.AsSpan(), value)
      buffer

    let binaryValue (value: obj) len littleEndian =
      let buffer = Array.zeroCreate len

      match value with
      | :? int16 as value when littleEndian = true -> BinaryPrimitives.WriteInt16LittleEndian(buffer.AsSpan(), value)
      | :? int16 as value -> BinaryPrimitives.WriteInt16BigEndian(buffer.AsSpan(), value)
      | :? int32 as value when littleEndian = true -> BinaryPrimitives.WriteInt32LittleEndian(buffer.AsSpan(), value)
      | :? int32 as value -> BinaryPrimitives.WriteInt32BigEndian(buffer.AsSpan(), value)
      | :? int64 as value when littleEndian = true -> BinaryPrimitives.WriteInt64LittleEndian(buffer.AsSpan(), value)
      | :? int64 as value -> BinaryPrimitives.WriteInt64BigEndian(buffer.AsSpan(), value)
      | :? double as value when littleEndian = true -> BinaryPrimitives.WriteDoubleLittleEndian(buffer.AsSpan(), value)
      | :? double as value -> BinaryPrimitives.WriteDoubleBigEndian(buffer.AsSpan(), value)
      | _ -> raise (ArgumentException($"A binary matcher for type '{value.GetType().FullName}' is not supported"))

      buffer

  open Helpers

  let inline int16Le (eq: int16) =
    binaryValue eq 2 true |> Match.byteSequence

  let inline int16Be (eq: int16) =
    binaryValue eq 2 false |> Match.byteSequence

  let inline int32Le (eq: int32) =
    binaryValue eq 4 true |> Match.byteSequence

  let inline int32Be (eq: int32) =
    binaryValue eq 4 false |> Match.byteSequence

  let inline int64Le (eq: int64) =
    binaryValue eq 8 true |> Match.byteSequence

  let inline int64Be (eq: int64) =
    binaryValue eq 8 false |> Match.byteSequence

  let inline doubleLe (eq: double) =
    binaryValue eq 8 true |> Match.byteSequence

  let inline doubleBe (eq: double) =
    binaryValue eq 8 false |> Match.byteSequence

module Materialize =

  let inline toByte () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) = span[0] }
    |> step

  let inline toMemory () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) = span.ToArray() |> ReadOnlyMemory }
    |> squash

  let inline toReadOnlySequence () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) = span.ToArray() |> ReadOnlySequence }
    |> squash

  let inline toDoubleLe () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) =
          BinaryPrimitives.ReadDoubleLittleEndian(span) }
    |> step

  let inline toInt16Le () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) =
          BinaryPrimitives.ReadInt16LittleEndian(span) }
    |> step

  let inline toInt32Le () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) =
          BinaryPrimitives.ReadInt32LittleEndian(span) }
    |> step

  let inline toInt64Le () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) =
          BinaryPrimitives.ReadInt64LittleEndian(span) }
    |> step

  let inline toDoubleBe () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) =
          BinaryPrimitives.ReadDoubleBigEndian(span) }
    |> step

  let inline toInt16Be () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) =
          BinaryPrimitives.ReadInt16BigEndian(span) }
    |> step

  let inline toInt32Be () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) =
          BinaryPrimitives.ReadInt32BigEndian(span) }
    |> step

  let inline toInt64Be () =
    { new IMaterializerConverter<_> with
        override __.Materialize(span) =
          BinaryPrimitives.ReadInt64BigEndian(span) }
    |> step

module Parsers =

  let inline int16Le () =
    ("int16Le", Match.bySize 2)
    /-> Materialize.toInt16Le

  let inline int16Be () =
    ("int16Be", Match.bySize 2)
    /-> Materialize.toInt16Be

  let inline int32Le () =
    ("int32Le", Match.bySize 4)
    /-> Materialize.toInt32Le

  let inline int32Be () =
    ("int32Be", Match.bySize 4)
    /-> Materialize.toInt32Be

  let inline int64Le () =
    ("int64Le", Match.bySize 8)
    /-> Materialize.toInt64Le

  let inline int64Be () =
    ("int64Be", Match.bySize 8)
    /-> Materialize.toInt64Be

  let inline doubleLe () =
    ("doubleLe", Match.bySize 8)
    /-> Materialize.toDoubleLe

  let inline doubleBe () =
    ("doubleBe", Match.bySize 8)
    /-> Materialize.toDoubleBe
