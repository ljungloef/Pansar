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

open Pansar
open Pansar.Buffers
open System.Buffers
open System.Text
open System
open System.Linq
open FsUnit.Xunit
open MaterializerFactory

module MemSegment =

  type MemSegment<'T>(buffer: 'T array) as this =
    inherit ReadOnlySequenceSegment<'T>()
    do this.Memory <- buffer |> ReadOnlyMemory

    member this.Chain(buffer: 'T array) =
      let segment =
        MemSegment(buffer, RunningIndex = this.RunningIndex + int64 this.Memory.Length)

      this.Next <- segment
      segment

  let tryDivideIntoSegments count (array: 'a array) =
    if array.Length / count < 1 then
      array |> ReadOnlySequence
    else
      let size =
        double array.Length / double count
        |> Math.Floor
        |> Convert.ToInt32

      let a = array |> Array.take size
      let b = array |> Array.skip size |> Array.take size

      let c =
        array
        |> Array.skip (size * 2)
        |> Array.take (array.Length - size * 2)

      let firstSegment = MemSegment(a)
      let lastSegment = firstSegment.Chain(b).Chain(c)

      ReadOnlySequence(firstSegment, 0, lastSegment, lastSegment.Memory.Length)

[<AutoOpen>]
module Helpers =

  let shouldBeFailed result =
    match result with
    | Failure (_, f) -> f
    | Consume _ -> failwithf "Should have failed"

  let shouldBeEndOfSequence result =
    match result with
    | Failure (_, (UnexpectedEndOfSeq pos)) -> pos
    | _ -> failwithf "Should have been eos but actually %A" result

  let materialize sequence result =
    match result with
    | Consume (materializer, _) -> MaterializerFunc.run materializer &sequence
    | Failure (tag, code) ->
      match code with
      | Unmatch -> failwith $"{tag}=Unmatch"
      | UnexpectedEndOfSeq _ -> failwith $"{tag}=End of sequence"
      | Unknown s -> failwithf $"{tag}=Failure: %s{s}"
      | Aggregated f -> failwithf $"{tag}=Failures %A{f}" f

  let resultParts (materializerFactory: MaterializerFactory<_, _>) tracker seq =
    let materializer = materializerFactory.Many(&tracker)

    let consumed = MaterializerFunc.run materializer &seq

    let remaining =
      seq
      |> ReadOnlySequence.sliceAt (seq.GetPosition(tracker.Length, tracker.StartPos))
      |> ReadOnlySequence.toArray

    (consumed, remaining)

  let materializeFindResult (materializerFactory: MaterializerFactory<_, _>) pos length seq =
    let materializer = materializerFactory.Single(pos, length)
    MaterializerFunc.run materializer &seq

  let seqEqual (a: 'a seq) (b: 'a seq) =
    let res = Enumerable.SequenceEqual(a, b)
    res |> should equal true

  let remainings (sequence: ReadOnlySequence<byte>) result =
    match result with
    | Consume (_, pos) -> sequence.Slice(pos)
    | Failure _ -> failwithf "Cannot get remainings from a failed result."

  let splitResult sequence =
    function
    | Consume (materializer, pos) -> (MaterializerFunc.run materializer &sequence, sequence.Slice(pos))
    | Failure (_, code) -> failwithf "Cannot get result from a failed result %A." code

  let utf8Bytes (s: string) = s |> Encoding.UTF8.GetBytes

  let sequenceOf (s: string) =
    s
    |> utf8Bytes
    |> MemSegment.tryDivideIntoSegments 3

  let sequenceToString (seq: ReadOnlySequence<byte>) =
    seq.ToArray() |> Encoding.UTF8.GetString

  let stringMaterializer () =
    { new IMaterializerConverter<_> with
        override __.Materialize(seq) =
          seq.ToArray() |> Encoding.UTF8.GetString }
    |> squash

  let singleByteMaterializer () =
    { new IMaterializerConverter<_> with
        override __.Materialize(seq) = seq.[0] }
    |> step

  let byteMaterializer () =
    { new IMaterializerConverter<_> with
        override __.Materialize(seq) = seq.ToArray() }
    |> squash

  let sequenceFromHexString (s: string) =
    s.Replace("-", String.Empty)
    |> Convert.FromHexString
    |> ReadOnlySequence
