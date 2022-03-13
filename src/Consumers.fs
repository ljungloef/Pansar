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

open ParserUtils

type Consumer<'T0, 'T1, 'M when 'M :> MaterializerFactory<'T0, 'T1>>
  (
    tag: ParserTag,
    matcher: IMatcher,
    materializer: 'M
  ) =
  inherit ParserBase<'T0>(tag)

  /// The `MaterializerFactory` that should be used to construct `IMaterializerFunc{}`s
  member __.Materializer = materializer

  /// The `IMatcher` associated to this consumer
  member __.Matcher = matcher

  /// Create a new tracker, based on the tracking settings for the consumer and `MaterializerFactory`.
  member this.MakeTracker(init) = this.Materializer.MakeTracker(init)

  override this.Parse(ctx, o) =

    let reader = &ctx.Reader
    let pre = reader.Remaining

    let mutable tracker = this.MakeTracker(reader.Position)
    let hadEnoughData = this.Matcher.Match(&reader, &tracker)

    if tracker.Matches < 1 then
      error
        &ctx
        this.Tag
        (if hadEnoughData then
           Unmatch
         else
           UnexpectedEndOfSeq reader.Sequence.End)
    else
      success &o (this.Materializer.Single(tracker.StartPos, int (pre - reader.Remaining)))

[<AutoOpen>]
module Consumer =

  /// Binds the matcher and materializer into a new Consumer. The parser tag will be unset. To also set the tag
  /// during the creation of the consumer, use `/->` instead.
  let inline (/>) (matcher: #IMatcher) materializer =
    Consumer((ParserTag "n/a"), matcher, (materializer ()))

  /// Binds the matcher and materializer into a new Consumer, using the given `tag` as parser tag of the consumer.
  let inline (/->) ((tag, matcher): string * #IMatcher) materializer =
    Consumer((ParserTag tag), matcher, (materializer ()))
