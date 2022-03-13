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

module ``parse using http headers parser`` =

  let httpHeadersParser =
    HttpHeadersParser.get
    |>> (fun ((method, uri, version), headers) -> (method, uri, version, headers |> dict))

  [<Fact>]
  [<Trait("Category", "HTTP")>]
  let ``should parse single request`` () =
    let content =
      """GET / HTTP/1.1
Host: localhost:8000
User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:98.0) Gecko/20100101 Firefox/98.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
Pragma: no-cache
Cache-Control: no-cache
"""
      |> sequenceOf

    let (method, uri, version, headers) =
      content |> Parse.withException httpHeadersParser

    method |> should equal "GET"
    uri |> should equal "/"
    version |> should equal "1.1"
    headers.["Host"] |> should equal "localhost:8000"

    headers.["User-Agent"]
    |> should equal "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:98.0) Gecko/20100101 Firefox/98.0"

    headers.["Accept"]
    |> should equal "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"

    headers.["Accept-Language"]
    |> should equal "en-US,en;q=0.5"

    headers.["Accept-Encoding"]
    |> should equal "gzip, deflate"

    headers.["Connection"]
    |> should equal "keep-alive"

    headers.["Pragma"] |> should equal "no-cache"

    headers.["Cache-Control"]
    |> should equal "no-cache"

  [<Fact>]
  [<Trait("Category", "HTTP")>]
  let ``should parse two requests`` () =
    let content =
      """GET / HTTP/1.1
Host: localhost:8000
User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:98.0) Gecko/20100101 Firefox/98.0
Accept: text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
Pragma: no-cache
Cache-Control: no-cache

GET /css/site.css HTTP/1.1
Host: localhost:8000
User-Agent: Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:98.0) Gecko/20100101 Firefox/98.0
Accept: text/css,*/*;q=0.1
Accept-Language: en-US,en;q=0.5
Accept-Encoding: gzip, deflate
Connection: keep-alive
Referer: http://localhost:8000/in
Pragma: no-cache
Cache-Control: no-cache
"""
      |> sequenceOf

    let res =
      content
      |> Parse.withException (whileConsuming1 httpHeadersParser)

    let (method1, uri1, version1, headers1) = res.[0]
    let (method2, uri2, version2, headers2) = res.[1]

    method1 |> should equal "GET"
    uri1 |> should equal "/"
    version1 |> should equal "1.1"
    headers1.["Host"] |> should equal "localhost:8000"

    headers1.["User-Agent"]
    |> should equal "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:98.0) Gecko/20100101 Firefox/98.0"

    headers1.["Accept"]
    |> should equal "text/html,application/xhtml+xml,application/xml;q=0.9,image/avif,image/webp,*/*;q=0.8"

    headers1.["Accept-Language"]
    |> should equal "en-US,en;q=0.5"

    headers1.["Accept-Encoding"]
    |> should equal "gzip, deflate"

    headers1.["Connection"]
    |> should equal "keep-alive"

    headers1.["Pragma"] |> should equal "no-cache"

    headers1.["Cache-Control"]
    |> should equal "no-cache"

    method2 |> should equal "GET"

    uri2 |> should equal "/css/site.css"

    version2 |> should equal "1.1"

    headers2.["Host"] |> should equal "localhost:8000"

    headers2.["User-Agent"]
    |> should equal "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:98.0) Gecko/20100101 Firefox/98.0"

    headers2.["Accept"]
    |> should equal "text/css,*/*;q=0.1"

    headers2.["Accept-Language"]
    |> should equal "en-US,en;q=0.5"

    headers2.["Accept-Encoding"]
    |> should equal "gzip, deflate"

    headers2.["Connection"]
    |> should equal "keep-alive"

    headers2.["Referer"]
    |> should equal "http://localhost:8000/in"

    headers2.["Pragma"] |> should equal "no-cache"

    headers2.["Cache-Control"]
    |> should equal "no-cache"
