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

module HttpHeadersParser =

  let crlf = Match.char '\n'
  let ws = Match.char ' '
  let colon = Match.char ':'
  let str = utf8String

  let headerKeyValue =
    // "Host: www.host.com"
    //
    // Result = ("Host", "www.host.com")
    slice2 colon (str (), utf8StringTrimStart ' ')

  let httpVersion =
    // "HTTP/1.1"
    //
    // Result = "1.1"
    Match.string "HTTP/" /> Materialize.ignore
    .> str ()

  let headers =
    // "Host: www.host.com
    // User-Agent: Mozilla/5.0
    // ..."
    //
    // Result = [("Host", "www.host.com"); ("User-Agent", "Mozilla/5.0"); ...]
    sliceBy crlf headerKeyValue

  let requestLine =
    // "GET /path/to/resource HTTP/1.1"
    //
    // Result = ("GET", "/path/to/resource", "1.1")
    slice3 ws (str (), str (), httpVersion)

  /// A sample parser that parses a HTTP header document in the format of:
  ///
  /// "GET /path/to/resource HTTP/1.1
  /// Host: www.host.com
  /// User-Agent: Mozilla/5.0
  /// Accept: text/html,application/xhtml+xml,application/xml;q=0.9,*/*;q=0.8
  /// Accept-Language: en-us,en;q=0.5
  /// Accept-Encoding: gzip, deflate
  /// Connection: keep-alive"
  ///
  let get = leftSliceTill crlf requestLine <.> headers
