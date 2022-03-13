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
open System.Runtime.CompilerServices

[<AutoOpen>]
module ParseResults =

  /// The final result from a parse run
  [<Struct>]
  type ParseResult<'T> =

    /// `Consume` means that the parser were able to successfully consume all items
    /// The attached sequence position gives the last position consumed
    | Consume of Consume: struct (IMaterializerFunc<'T> * SequencePosition)

    /// `Failure` occurs when a parser is not able to consume the expected items.
    /// The reason to the failure is given in the `ParseFailureCode' of the case. The attached tag tells which parser that reported the failure
    | Failure of Failure: struct (ParserTag * ParseFailureCode)

  /// The available failure codes
  and [<Struct>] ParseFailureCode =

    /// The parser expected to consume more items but reached the end of the sequence.
    /// The final position is included.
    | UnexpectedEndOfSeq of EndOfSequence: SequencePosition

    /// The read items did not match what the parser expected
    | Unmatch

    /// An unspecified error occured
    | Unknown of Unknown: string

    /// Gives all encountered errors when a parser made multiple attempts
    | Aggregated of Aggregated: struct (ParserTag * ParseFailureCode) list

  /// Parser identifier
  and [<Struct; IsReadOnly>] ParserTag =

    /// Parser identifier
    | ParserTag of string

  /// An exception thrown when running a parse methods, instead of eval.
  exception ParseException of (ParserTag * ParseFailureCode)

[<AutoOpen>]
module ParserPrimitives =

  [<AbstractClass>]
  type ParserBase<'T>(tag: ParserTag) =
    let mutable tag = tag

    /// A tag identifying a specific parser instance. Can be changed using `Parsers.retag` or `parser <?> "new-tag"`
    member __.Tag
      with get () = tag
      and internal set (value) = tag <- value

    member internal self.Clone() = self.MemberwiseClone()

    /// Run the parser in the given context. The parser returns `false` upon any failure, and `true` when it could consume as expected.
    /// The parser should also set detailed error info in the context.
    /// The parser should always set a `IMaterializerFunc{T}` that can be used to materialize the consumed segment at a later time
    abstract member Parse: byref<ParseContext> * outref<IMaterializerFunc<'T>> -> bool

  /// The context is used to track the progress and result of a parse run
  and [<Struct; IsByRefLike>] ParseContext =

    /// The reader used to consume items. The reader's position should always, at start, be set from where the parser should start consuming
    val mutable Reader: SequenceReader<byte>

    /// Indicates if the parser runs in the main or a sub context.
    /// This can e.g. be used to avoid uneccessary position changes in sub/sliced contexts.
    val mutable IsMainContext: bool

    /// Indicates if a parser has reported an error
    val mutable HasError: bool

    /// Error details
    val mutable Error: ParseError

    new(reader: SequenceReader<byte>, isMainContext) =
      { Reader = reader
        HasError = false
        IsMainContext = isMainContext
        Error = ParseError.Empty() }

    /// Report back that an error occured
    member this.SetError(error: ParseError) =
      this.HasError <- true
      this.Error <- error
      false

    /// Clear the error and reset the fault state of the context.
    member this.ClearError() =
      this.HasError <- false
      this.Error <- ParseError.Empty()
      true

  and [<Struct; IsByRefLike>] ParseError(tag: ParserTag, code: ParseFailureCode) =

    /// The tag of the specific parser that reported the error
    member __.Tag = tag

    /// The actual failure reported by the parser
    member __.Code = code

    static member Empty() = ParseError(ParserTag "", Unknown "")


  /// Parser without any defined behavior internally
  /// The supplied `IParserFunc{T}` will be used to parse.
  type Parser<'T>(tag, parser: IParserFunc<'T>) =
    inherit ParserBase<'T>(tag)

    override __.Parse(reader, o) = parser.Parse(&reader, tag, &o)

  and IParserFunc<'T> =
    /// Same as `ParserBase{T}.Parse` plus the addition of the current parsers tag.
    abstract member Parse: byref<ParseContext> * ParserTag * outref<IMaterializerFunc<'T>> -> bool


module ParserUtils =

  let inline parse (r: byref<ParseContext>) (p: #ParserBase<_>) = p.Parse &r

  let inline success (o: outref<'a>) (m: 'a) =
    o <- m
    true

  let inline upgrade (ctx: byref<ParseContext>) (o: outref<'a>) (m: 'a) =
    o <- m
    ctx.ClearError()

  let inline error (ctx: byref<ParseContext>) tag code = ctx.SetError(ParseError(tag, code))

  let inline copyError (from: byref<ParseContext>) (to': byref<ParseContext>) = to'.SetError(from.Error)

  let inline endOfSequenceError (ctx: byref<ParseContext>) tag =
    error &ctx tag (UnexpectedEndOfSeq ctx.Reader.Sequence.End)

  let inline ifSuccess (o: outref<IMaterializerFunc<_>>) ([<InlineIfLambda>] f) =
    function
    | false, _ -> false
    | true, mat -> success &o (mat |> f)

  let inline ifError (o: outref<IMaterializerFunc<_>>) ([<InlineIfLambda>] f) =
    function
    | false, _ -> f ()
    | true, mat -> success &o mat

  let inline return' (o: outref<IMaterializerFunc<_>>) =
    function
    | false, _ -> false
    | true, m -> success &o m
