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

namespace Pansar.Benchmarks.LeftSliceTill

open BenchmarkDotNet.Attributes
open System.Buffers
open System.IO
open Pansar
open Pansar.Text

[<SimpleJob; MemoryDiagnoser>]
type LeftSliceTillBenchmark() =

  let delimiter = Match.char '.' /> Materialize.toString
  let content = Parsers.utf8String ()

  let mutable data: ReadOnlySequence<byte> = ReadOnlySequence.Empty
  let mutable counter = 0

  [<Params("lorum-ipsum-1-p.txt")>]
  member val public file = "" with get, set

  [<GlobalSetup>]
  member self.Setup() =
    let file = Path.Combine(Directory.GetCurrentDirectory(), "Fixtures", self.file)

    data <- File.ReadAllBytes(file) |> ReadOnlySequence

  [<Benchmark>]
  member _.Run() =
    data
    |> Parse.eval (leftSliceTill delimiter content)
    |> ignore

  member _.RunVerified() =
    let result =
      data
      |> Parse.eval (leftSliceTill delimiter content)

    if counter = 0 then
      counter <- counter + 1

      match result with
      | Failure _ -> failwith "Verification failure"
      | Consume (mat, _) ->
        let words = MaterializerFunc.run mat &data
        assert (words.Length > 0)
