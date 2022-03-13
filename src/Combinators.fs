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

module Parsers =

  open ParserUtils

  /// Create a new parser that always materializes into the `value`
  let inline get value =
    Parser(
      ParserTag "get",
      { new IParserFunc<'a> with
          member __.Parse(_, _, o) = success &o (MaterializerFunc.get value) }
    )

  /// Create a new parser that always succeeds, without consuming any items. Equivalent to `get Void`
  let inline succeed () = get Void

  /// Create a new parser that immediately consumes all unread items in the sequence
  /// The internal reader will be positioned in the end of the sequence.
  let inline consume converter =
    Parser(
      ParserTag "consume",
      { new IParserFunc<'a> with
          member __.Parse(ctx, _, o) =
            let pre = ctx.Reader.Position
            let len = int ctx.Reader.Remaining

            if ctx.IsMainContext then
              ctx.Reader.AdvanceToEnd()

            success &o (MaterializerFunc.scoped converter pre len) }
    )

  /// Create a new parser that will always fail with the given `code`
  let inline failure code =
    Parser(
      ParserTag "failure",
      { new IParserFunc<_> with
          member __.Parse(ctx, tag, _) = error &ctx tag code }
    )

  /// Creates a new parser that will return `None` if the supplied `parser` fails; or `Some value` upon success
  let inline opt parser =
    Parser(
      ParserTag "opt",
      { new IParserFunc<'a option> with
          member __.Parse(ctx, _, o) =
            match parse &ctx parser with
            | false, _ -> success &o (MaterializerFunc.get None)
            | true, mat -> success &o (mat |> MaterializerFunc.map Some) }
    )

  /// Creates a new parser that will, upon success, apply he function `f` on the materialized value
  let inline map ([<InlineIfLambda>] f) parser =
    Parser(
      ParserTag "map",
      { new IParserFunc<'b> with
          member __.Parse(ctx, _, o) =
            let result = parse &ctx parser
            ifSuccess &o (MaterializerFunc.map f) result }
    )

  /// Creates a copy of the supplied parser with the new `tag`
  let retag<'a, 'b when 'a :> ParserBase<'b>> (tag: ParserTag) (p: 'a) =
    let clone = p.Clone() :?> 'a in
    clone.Tag <- tag
    clone

  /// Create a new parser that runs the `parser`. If the `parser` succeeds, the materialized value will be supplied to the function `f`.
  /// The function `f` evaluates to `materializedValue -> nextParser`
  /// The `nextParser` from `f` will be used to continue parsing from the current position.
  /// The result from `nextParser` will be the end result of this parser.
  let inline bind f parser =
    Parser(
      ParserTag "bind",
      { new IParserFunc<'b> with
          member __.Parse(ctx, _, o) =
            match parse &ctx parser with
            | false, _ -> false
            | true, mat ->
              let seq = ctx.Reader.Sequence
              let materializedValue = MaterializerFunc.run mat &seq
              let bindTo = materializedValue |> f

              let innerResult = parse &ctx bindTo
              return' &o innerResult }
    )

  /// Create a new parser that runs the `parser`. If the `parser` succeeds, the materialized value will be supplied to the function `f`.
  /// The function `f` evaluates to `materializedValue -> outref<IMaterializerFunc<_>> -> bool` and is assigned
  /// the responsibility to return the result of this parser.
  let inline assign f parser =
    Parser(
      ParserTag "assign",
      { new IParserFunc<'b> with
          member __.Parse(ctx, _, o) =
            match parse &ctx parser with
            | false, _ -> false
            | true, mat ->
              let seq = ctx.Reader.Sequence
              let materializedValue = MaterializerFunc.run mat &seq
              materializedValue |> f }
    )

  /// Create a new parser that will run parser `a`, followed by `b`, if `a` succeeded.
  /// If both are successful, the `combine` function will be called with both `IMaterializerFunc{}` from the results. The `combine` should return
  /// a new `IMaterializerFunc{}`, with the possbility to combine both values.
  let inline andThen a b ([<InlineIfLambda>] combine) =
    Parser(
      ParserTag "andThen",
      { new IParserFunc<'d> with
          member __.Parse(ctx, _, o) =
            match parse &ctx a with
            | true, matA ->
              let (result, matB) = parse &ctx b

              result && success &o (combine struct (matA, matB))
            | _ -> false }
    )

  /// Create a new parser that will run parser `a`, followed by `b`, only if `a` fails.
  /// Both parsers will start at the start positon of the reader, hence `b` will not continue from where `a` failed; it will start from the
  /// position of the parser was fisrt called.
  let inline orElse a b ([<InlineIfLambda>] mapA) ([<InlineIfLambda>] mapB) =
    Parser(
      ParserTag "orElse",
      { new IParserFunc<'c> with
          member __.Parse(ctx, _, o) =
            let copy = ctx.Reader

            match (parse &ctx a) with
            | true, matA -> success &o (matA |> mapA)
            | false, _ ->
              let reader = &ctx.Reader
              reader <- copy

              let resultB = parse &ctx b
              ifSuccess &o mapB resultB }
    )

[<AutoOpen>]
module Primitives =

  open Parsers

  /// `Parsers.map` in reverse parameter order
  let inline (|>>) x f = map f x

  /// `Parsers.retag` in reverse parameter order
  let (<?>) p t = retag t p

  /// `Parsers.bind` in reverse parameter order
  let inline (>>=) parser f = bind f parser

  /// `Parsers.assign` in reverse parameter order
  let inline (>>*) parser f = assign f parser

  /// `Parsers.andThen` where the result from `a` and `b` are combined in a tuple `('a * 'b)`
  let inline (<.>) a b =
    andThen a b (fun struct (a, b) -> MaterializerFunc.combine a b)

  /// `Parsers.andThen` where only the result from `a` is kept.
  /// The result from `b` will be discarded before materialization (meaning skipping all allocations for the value 'b)
  let inline (<.) a b = andThen a b (fun struct (a, _) -> a)

  /// `Parsers.andThen` where only the result from `b` is kept.
  /// The result from `a` will be discarded before materialization (meaning skipping all allocations for the value 'b)
  let inline (.>) a b = andThen a b (fun struct (_, b) -> b)

  /// `Parsers.andThen` where the results from both `a` and `b` are discarded before materialization, and replaced by a single `Void`.
  let inline (>.<) a b =
    andThen a b (fun struct (_, _) -> MaterializerFunc.get Void)

  /// `Parsers.orElse` where no results are discarded. The value will be kept in `Choice1Of2` if `a` succeeds and in
  /// `Choice2Of2` if `b` succeeds.
  let inline (<^>) a b =
    orElse a b (MaterializerFunc.map Choice<_, _>.Choice1Of2) (MaterializerFunc.map Choice<_, _>.Choice2Of2)

  /// `Parsers.orElse` where only the result from `a` is kept. The `b` parser will still run if `a` fails, but the result
  /// will be replaced by None.
  let inline (<^) a b =
    orElse a b (MaterializerFunc.map Option<'a>.Some) (fun _ -> MaterializerFunc.get Option<'a>.None)

  /// `Parsers.orElse` where only the result from `b` is kept. The `a` parser will return None if succeeding, and hence
  /// that `b` will not run at all in that case.
  let inline (^>) a b =
    orElse a b (fun _ -> MaterializerFunc.get Option<'b>.None) (MaterializerFunc.map Option<'b>.Some)

  /// `Parsers.orElse` where the results from both `a` and `b` are discarded, and replaced by a single Void.
  let inline (^^) a b =
    orElse a b (fun _ -> MaterializerFunc.get Void) (fun _ -> MaterializerFunc.get Void)

  /// Parse `a`, and then `b`, and map the result using `f`
  let inline pipe2 a b f = a <.> b |>> f

  /// Parse `a`, `b` and then `c` in order. Calls `f` with a flattened 3-tuple structure
  let inline pipe3 a b c f =
    a <.> b <.> c |>> (fun ((x, y), z) -> f (x, y, z))

  /// Parse `a`, `b` 'c' and then `d` in order. Calls `f` with a flattened 4-tuple structure
  let inline pipe4 a b c d f =
    a <.> b <.> c <.> d
    |>> (fun (((x, y), z), r) -> f (x, y, z, r))

  /// Apply the parser `a` two times, and keep the result from both
  let inline consume2 a = a <.> a

  /// Apply the parser `a` three times, and keep the result from all three
  let inline consume3 a = a <.> a <.> a

  /// Apply the parser `a` four times, and keep the result from all four
  let inline consume4 a = a <.> a <.> a <.> a

  /// Parse `a` and then `b` in order. The result from `a` is concatenated with the result from `b`
  let inline con a b =
    a <.> b |>> fun (lst, sng) -> lst @ [ sng ]

  let inline (<@>) a b = con a b
