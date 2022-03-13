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
open System
open System.Text

module Debug =

  let utf8 (b: byte array) = b |> Encoding.UTF8.GetString

  let logContentBetween (reader: byref<SequenceReader<byte>>) (start: SequencePosition) (stop: SequencePosition) ctx =
    let s = reader.Sequence.Slice(start, stop).ToArray()
    printfn "Slice %s; len=%i and '%s'" ctx s.Length (utf8 s)
