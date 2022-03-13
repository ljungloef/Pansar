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

open Pansar.Buffers
open System.Buffers
open ParserUtils

#nowarn "64"

module Delimiters =

  module Resolver =

    let rec naiveDelimiterSearcher (parser: Parser<_>) (ctx: byref<ParseContext>) (length: outref<int64>) count =
      let pre = ctx.Reader

      match parse &ctx parser with
      | false, _ ->
        match ctx.Error.Code with
        | UnexpectedEndOfSeq _ when count = 0 -> FindResult.NotEnoughData
        | UnexpectedEndOfSeq _ -> FindResult.NotFound
        | _ -> naiveDelimiterSearcher parser &ctx &length (count + 1)
      | true, _ ->
        length <- ctx.Reader.Remaining - pre.Remaining
        ctx.Reader <- pre
        FindResult.Found

    type Resolver = Resolve
      with
        static member inline ResolveDelimiterSearcher(_: Resolver, consumer: Consumer<_, _, _>) : IDelimiterSearcher =
          consumer.Matcher

        static member inline ResolveDelimiterSearcher(_: Resolver, matcher: #IMatcher) : IDelimiterSearcher = matcher

        static member inline ResolveDelimiterSearcher(_: Resolver, parser: Parser<_>) : IDelimiterSearcher =
          { new IDelimiterSearcher with
              member __.FindNext(reader, o) =
                let mutable ctx = ParseContext(reader, false)
                naiveDelimiterSearcher parser &ctx &o 0 }

  let inline resolveDelimiterArg (arg: ^x) : IDelimiterSearcher =
    ((^b or ^x): (static member ResolveDelimiterSearcher: ^b * ^x -> _) (Resolver.Resolve, arg))

[<AutoOpen>]
module SlicingCombinators =

  module Helpers =

    let inline consumerError (ctx: byref<ParseContext>) tag hadEnoughData =
      let code =
        if hadEnoughData then
          Unmatch
        else
          (UnexpectedEndOfSeq ctx.Reader.Sequence.End)

      error &ctx tag code

  /// Options to control the behavior of the `leftSliceTill` operation
  type LeftSliceTillOptions =
    {
      /// Given the sequence `[| 0; 0; 0; 1; 0 |]`, and `delimiter=1`, the `IncludeSepInSlice` determines if the `1` should be included in the resulting slice
      /// given to the assigned parser. Following the same example, setting `IncludeSepInSlice` to `true` would include `1`, while `false` would exclude `1` from the slice.
      IncludeSepInSlice: bool }
    static member Default = { IncludeSepInSlice = false }

  /// Apply the `parser` on a sliced out part of the underlying sequence. The `opts` can be used to control the behavior
  let inline leftSliceTillOpts delimiter parser (opts: LeftSliceTillOptions) =
    Parser(
      ParserTag "leftSliceTill",
      { new IParserFunc<_> with
          member __.Parse(ctx, tag, o) =
            let startPos = ctx.Reader.Position

            let searcher = Delimiters.resolveDelimiterArg delimiter

            match searcher.FindNext(&ctx.Reader) with
            | (FindResult.NotEnoughData, _) -> Helpers.consumerError &ctx tag false
            | (FindResult.NotFound, _) -> Helpers.consumerError &ctx tag true
            | (FindResult.Found, delimiterLength) ->

              let endPos =
                if opts.IncludeSepInSlice then
                  ctx.Reader.Sequence.GetPosition(delimiterLength, ctx.Reader.Position)
                else
                  ctx.Reader.Position

              let sliceReader = SequenceReader.sub &ctx.Reader startPos endPos
              let mutable innerCtx = ParseContext(sliceReader, false)

              let result = parse &innerCtx parser

              // Advance past the delimiter before continuing even if the content parser failed. Otherwise the
              // reader will be positioned in a most likely unwanted position (just before the delimiter).
              ctx.Reader.Advance(delimiterLength)

              match result with
              | false, _ -> copyError &innerCtx &ctx
              | true, mat -> success &o mat

            | _ -> error &ctx tag (Unknown "Unexpected result.") }
    )

  /// Apply the `parser` on a sliced out part of the underlying sequence with default options
  let inline leftSliceTill delimiter parser =
    leftSliceTillOpts delimiter parser LeftSliceTillOptions.Default

  /// Forms a slice from current position, with `length` items.
  let leftSlice (length: int) parser =
    Parser(
      ParserTag "leftSlice",
      { new IParserFunc<'b> with
          member __.Parse(ctx, tag, o) =
            if ctx.Reader.Remaining < length then
              endOfSequenceError &ctx tag
            else
              let sliced =
                ctx.Reader.Sequence
                |> ReadOnlySequence.sliceFrom ctx.Reader.Position length

              let mutable innerCtx = ParseContext(SequenceReader(sliced), false)

              match parse &innerCtx parser with
              | false, _ -> copyError &innerCtx &ctx
              | true, mat ->
                ctx.Reader.Advance(length)
                success &o mat }
    )

  /// Options to control the behavior of the `sliceBy` combinator
  type SliceByOptions =
    {
      /// Slice out at maximum `Limit` slices. Use `None` for unlimited. The default is `None`
      Limit: int option }

    /// Create a new `SliceByOptions` with default value set
    static member Default = { Limit = None }

  /// Continously slice the sequence by `delimiter` and apply the `parser` on the left slice, until the parser fails or the limit is reached.
  let inline sliceByOpts delimiter parser (opts: SliceByOptions) =
    let whileOpts =
      { WhileConsumingOptions.Default with
          Min = 1
          Limit = opts.Limit }

    whileOpts
    |> whileConsumingOpts (leftSliceTill delimiter parser)

  /// Forms a slice based on the predicate, and applies `parser` on the slice
  let inline leftSliceWhile predicate parser =
    raise (System.NotImplementedException())

  /// Continously slice the sequence by `delimiter` and apply the `parser` on the left slice, until either parser fails.
  let inline sliceBy delimiter parser =
    sliceByOpts delimiter parser SliceByOptions.Default

  /// Form two slices, seperated by `delimiter`, and apply `a` on the first slice, and `b` on the second slice.
  /// The `delimiter` is excluded from both slices.
  let inline slice2 delimiter (a, b) =
    leftSliceTill delimiter a <.> b
    <?> ParserTag "slice2"

  /// Form three slices, seperated by `delimiter`, and apply `a` on the first slice, `b` on the second slice and `c` on the third slice.
  /// The `delimiter` is excluded from all slices.
  let inline slice3 delimiter (a, b, c) =
    leftSliceTill delimiter a
    <.> leftSliceTill delimiter b
    <.> c
    |>> (fun ((a, b), c) -> (a, b, c))
    <?> ParserTag "slice3"

  /// Form four slices, seperated by `delimiter`, and apply `a` on the first slice, `b` on the second slice, `c` on the third slice and `d` on the fourth slice.
  /// The `delimiter` is excluded from all slices.
  let inline slice4 delimiter (a, b, c, d) =
    leftSliceTill delimiter a
    <.> leftSliceTill delimiter b
    <.> leftSliceTill delimiter c
    <.> d
    |>> (fun (((a, b), c), d) -> (a, b, c, d))
    <?> ParserTag "slice4"

  /// Form four slices, seperated by `delimiter`, and apply `a` on the first slice, `b` on the second slice, `c` on the third slice, `d` on the fourth slice and `e` on the fifth slice.
  /// The `delimiter` is excluded from all slices.
  let inline slice5 delimiter (a, b, c, d, e) =
    leftSliceTill delimiter a
    <.> leftSliceTill delimiter b
    <.> leftSliceTill delimiter c
    <.> leftSliceTill delimiter d
    <.> e
    |>> (fun ((((a, b), c), d), e) -> (a, b, c, d, e))
    <?> ParserTag "slice5"

  module SliceSomeDefinition =

    type SliceSomeDefinition<'T, 'P when 'P :> ParserBase<'T>> = { Parser: 'P }

    // Starting point when building a new `SliceSomeDefinition`. The result of `a` will be included by left slicing.
    // To build a `SliceSomeDefinition` that exludes the start, use `defineExcl`.
    let inline define (delimiter, a) =
      { Parser = (leftSliceTill delimiter a) }

    // Starting point when building a new `SliceSomeDefinition`. The items before the delimiter will be excluded.
    // To build a `SliceSomeDefinition` that includes the start, use `define`.
    let inline defineExcl delimiter =
      { Parser = (leftSliceTill delimiter (Parsers.succeed ())) }

    /// Apply the parser `b` on the left side of the delimiter, and include the result.
    let inline (|=) (a: SliceSomeDefinition<_, _>) (delimiter, b) =
      { Parser = a.Parser <.> (leftSliceTill delimiter b) }

    /// Apply the parser `b` on the left side of the delimiter, but discard the result.
    let inline (|.) (a: SliceSomeDefinition<_, _>) delimiter =
      { Parser =
          a.Parser
          <. (leftSliceTill delimiter (Parsers.succeed ())) }

    /// Apply the parser `b` on the rest of the items in the sequence.
    let inline rest b def = { Parser = def.Parser <.> b }

  open SliceSomeDefinition

  /// Creates a parser that will slice and apply according to a `SliceDefinition`
  /// Create a new definiton by using `define` in the `SliceSomeDefinition` module.
  let inline sliceSome (definition: SliceSomeDefinition<_, _>) = definition.Parser
