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

#nowarn "64"

[<AutoOpen>]
module ControlCombinators =

  open ParserUtils

  [<AutoOpen>]
  module WhileConsuming =

    /// Options to control the behavior of the `whileConsuming` combinator
    type WhileConsumingOptions =
      { /// Consume at least `Min` times, or the operation will fail. Default is 0
        Min: int

        /// Consume at maximum `Limit` times. Use `None` for unlimited. The default is `None`
        Limit: int option }

      /// Create a new `WhileConsumingOptions` with default value set
      static member Default = { Min = 0; Limit = None }

    module Helpers =

      let rec internal applyWhileConsuming
        (ctx: byref<ParseContext>)
        parser
        (completed: IMaterializerFunc<'b> list)
        min
        max
        (o: outref<IMaterializerFunc<_>>)
        =
        match parse &ctx parser with
        | false, _ when completed.Length < min -> false
        | false, _ -> upgrade &ctx &o (MaterializerFunc.combineMany completed)
        | true, mat when max <> -1 && completed.Length + 1 >= max ->
          success &o (MaterializerFunc.combineMany (mat :: completed))
        | true, mat -> applyWhileConsuming &ctx parser (mat :: completed) min max &o

    type WhileConsumingCombinator = WhileConsumingCombinator
      with

        /// Runs the `parser` until it fails, or the limit set by `opts` is met.
        static member WhileConsuming(_: WhileConsumingCombinator, parser: Parser<_>, opts: WhileConsumingOptions) =
          let min = opts.Min
          let limit = opts.Limit |> Option.defaultValue -1

          Parser(
            ParserTag "whileConsuming",
            { new IParserFunc<'a array> with
                member __.Parse(ctx, _, o) =
                  Helpers.applyWhileConsuming &ctx parser [] min limit &o }
          )

        /// Runs the `parser` until it fails, or the limit set by `opts` is met.
        static member WhileConsuming
          (
            _: WhileConsumingCombinator,
            parser: Consumer<_, _, _>,
            opts: WhileConsumingOptions
          ) =
          Parser(
            ParserTag "whileConsuming",
            { new IParserFunc<_> with
                member __.Parse(ctx, _, o) =
                  let mutable tracker = parser.MakeTracker(ctx.Reader.Position)
                  let hadEnoughData = parser.Matcher.MatchMany(&ctx.Reader, &tracker)

                  if tracker.Matches < opts.Min then
                    let code =
                      if hadEnoughData then
                        Unmatch
                      else
                        UnexpectedEndOfSeq ctx.Reader.Sequence.End

                    error &ctx parser.Tag code
                  else
                    success &o (parser.Materializer.Many(&tracker)) }
          )

    /// Runs the `parser` until it fails, but the combinator itself, will always succeed.
    let inline whileConsuming (parser: ^p) =
      ((^a or ^p): (static member WhileConsuming: ^a * ^p * _ -> _) (WhileConsumingCombinator,
                                                                     parser,
                                                                     WhileConsumingOptions.Default))

    /// Runs the `parser` until it fails, but requires at least one match in order to succeed.
    let inline whileConsuming1 (parser: ^p) =
      ((^a or ^p): (static member WhileConsuming: ^a * ^p * _ -> _) (WhileConsumingCombinator,
                                                                     parser,
                                                                     { WhileConsumingOptions.Default with Min = 1 }))

    /// Runs the `parser` until it fails, controlled by the options in `opts`
    let inline whileConsumingOpts (parser: ^p) (opts: WhileConsumingOptions) =
      ((^a or ^p): (static member WhileConsuming: ^a * ^p * _ -> _) (WhileConsumingCombinator, parser, opts))

  // Tries to run the `parser` exactly `times`. The operation is aborted if the `parser` fails.
  let inline exactly times parser =
    whileConsumingOpts
      parser
      { WhileConsumingOptions.Default with
          Min = times
          Limit = Some times }

  [<AutoOpen>]
  module Choose =

    module Helpers =

      let rec chooseIter
        (outerCtx: byref<ParseContext>)
        tag
        (parser, branch)
        rest
        failures
        (o: outref<IMaterializerFunc<_>>)
        =

        let mutable ctx =
          ParseContext(SequenceReader.fromSeq outerCtx.Reader.Sequence, true)

        match parse &ctx parser with
        | true, _ ->
          match parse &ctx branch with
          | true, mat -> success &o mat
          | _ -> copyError &ctx &outerCtx
        | _ ->
          let aggregated = struct (branch.Tag, ctx.Error.Code) :: failures

          match rest with
          | [] -> error &outerCtx tag (Aggregated aggregated)
          | head :: tail -> chooseIter &outerCtx tag head tail failures &o

    open Helpers

    /// Test multiple parser pairs `(a * b)`. The `b` parser of the first pair with an `a` parser that succeeds will be used
    /// for parsing.
    let inline choose pairs =
      match pairs with
      | [] -> failwith "Choose requires at least one parser"
      | head :: tail ->
        Parser(
          ParserTag "choose",
          { new IParserFunc<'a> with
              member __.Parse(ctx, tag, o) = chooseIter &ctx tag head tail [] &o }
        )
