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

namespace Pansar.Text

open System
open System.Text
open Pansar
open MaterializerFactory
open System.Buffers
open System.Buffers.Text
open Pansar.Buffers
open ParserUtils

#nowarn "3391"

type NumberFormat =
  | Default = ' '
  | General = 'g'
  | Decimal = 'd'
  | Number = 'n'
  | Hexadecimal = 'x'

module Predicates =

  module Helpers =

    let inline success l (len: byref<int64>) =
      len <- l
      true

    let inline isTwoOctets (span: ReadOnlySpan<byte>) =
      ReadOnlySpan.atLeast span 2
      && (span[0] &&& 0xC0uy = 0xC0uy)

  open Helpers

  module Ascii =

    module Codes =
      let hex11Upper = byte 'A'
      let hex11Lower = byte 'a'
      let hex16Upper = byte 'F'
      let hex16Lower = byte 'f'

      let space = byte '\u0020'
      let vtab = byte '\t'
      let dot = byte '.'

    let inline isLowerCase v = v >= 97uy && v <= 122uy
    let inline isUpperCase v = v >= 65uy && v <= 90uy
    let inline isNumeric v = v >= 48uy && v <= 57uy

    let inline isHexadecimal v =
      isNumeric v
      || (v >= Codes.hex11Upper && v <= Codes.hex16Upper)
      || (v >= Codes.hex11Lower && v <= Codes.hex16Lower)

    let inline isLetter v = isLowerCase v || isUpperCase v

    let inline isAlphaNumeric v =
      isLowerCase v || isUpperCase v || isNumeric v

    let inline isSpace v = v = Codes.space
    let inline isHorizontalWhitespace v = v = 9uy || isSpace v
    let inline isWhitespace v = (v >= 9uy && v <= 13uy) || isSpace v

  module Utf8 =

    let inline testRune (span: ReadOnlySpan<byte>) ([<InlineIfLambda>] runePredicate: Rune -> bool) =
      let mutable rune = Unchecked.defaultof<Rune>
      let mutable len = Unchecked.defaultof<int>

      Rune.DecodeFromUtf8(span, &rune, &len) = OperationStatus.Done
      && runePredicate rune

    let inline isAscii v = v <= 0x7Fuy

    let inline predicate
      ([<InlineIfLambda>] isAsciiSpecific: byte -> bool)
      ([<InlineIfLambda>] runePredicate: Rune -> bool)
      =
      { new IRangePredicate with

          override __.MinLength = 1
          override __.MaxLength = 4

          override __.Predicate =
            { new ISpanPredicate with
                override __.IsMatch(span, len) =
                  if isAscii span[0] then
                    if isAsciiSpecific span[0] then
                      success 1 &len
                    else
                      false
                  elif isTwoOctets span && testRune span runePredicate then
                    success 2 &len
                  else
                    false } }


    let inline isLowerCase () =
      predicate Ascii.isLowerCase Rune.IsLower

    let inline isUpperCase () =
      predicate Ascii.isUpperCase Rune.IsUpper

    let inline isNumeric () = predicate Ascii.isNumeric Rune.IsNumber

    let inline isLetter () = predicate Ascii.isLetter Rune.IsLetter

    let inline isAlphaNumeric () =
      predicate Ascii.isAlphaNumeric Rune.IsLetterOrDigit

    let isSpace = Ascii.isSpace

    let inline isWhitespace () =
      predicate Ascii.isWhitespace Rune.IsWhiteSpace

[<RequireQualifiedAccess>]
module Match =

  module Helpers =

    let inline encodeChars (c: char seq) =
      c |> Seq.toArray |> Encoding.UTF8.GetBytes

    let inline encodeString (s: string) = s |> Encoding.UTF8.GetBytes

  open Helpers

  let inline string (eq: string) =
    eq |> encodeString |> Match.byteSequence

  let inline charSequence (seq: char seq) =
    seq |> encodeChars |> Match.byteSequence

  let inline char (eq: char) : IMatcher =
    let bytes = encodeChars [| eq |]

    if bytes.Length = 1 then
      Match.byte bytes[0]
    else
      Match.byteSequence bytes

  let inline eitherChar (set: char seq) =
    let converted =
      set
      |> Seq.choose (fun c ->
        try
          Some(Convert.ToByte(c))
        with
        | _ -> None)

    if Seq.length converted <> Seq.length set then
      raise (FormatException($"'eitherChar' only supports single byte chars"))

    converted |> Seq.toArray |> Match.byteSequence

module Materialize =

  module Helpers =

    let inline convert (span: ReadOnlySpan<byte>) = Encoding.UTF8.GetString(span)

    let inline stringConverter () =
      { new IMaterializerConverter<_> with
          override __.Materialize(span) = convert span }

  open Helpers

  let inline toString () = stringConverter () |> squash

module Parsers =

  let inline utf8String () =
    Materialize.Helpers.stringConverter ()
    |> Parsers.consume

  module Helpers =

    let inline getSpan (seq: ReadOnlySequence<byte>) max =
      if seq.IsSingleSegment then
        seq.FirstSpan
      else
        ReadOnlySequence.copyToStackalloc seq max

    let inline getSpanFromAvailable (reader: byref<SequenceReader<byte>>) (max: int) =
      let remaining = int reader.Remaining

      let rest =
        reader.Sequence.Slice(
          reader.Position,
          if max > remaining then
            remaining
          else
            max
        )

      getSpan rest max

    let inline validateFormat supportedFormats format =
      if supportedFormats
         |> Array.tryFind (fun f -> f = format)
         |> Option.isNone then
        raise (FormatException($"'{format}' is an invalid format"))
      else
        format

  open Helpers

  let inline utf8ValueParser< ^x, ^v when ^x: (static member TryParse:
    ReadOnlySpan<byte> * byref< ^v > * byref<int> * char -> bool)>
    tag
    (format: NumberFormat)
    =

    let charFormat =
      LanguagePrimitives.EnumToValue format
      |> validateFormat [| '\000'
                           'g'
                           'd'
                           'n'
                           'x' |]

    Parser(
      ParserTag tag,
      { new IParserFunc< ^v > with
          member __.Parse(ctx, tag, o) =
            if ctx.Reader.Remaining < 1 then
              endOfSequenceError &ctx tag
            else
              let mutable result = Unchecked.defaultof< ^v>
              let mutable bytesConsumed = Unchecked.defaultof<int>

              let span = getSpanFromAvailable &ctx.Reader 128

              let parseResult =
                (^x: (static member TryParse: ReadOnlySpan<byte> * byref< ^v > * byref<int> * char -> bool) (span,
                                                                                                             &result,
                                                                                                             &bytesConsumed,
                                                                                                             charFormat))

              match parseResult with
              | true ->
                ctx.Reader.Advance(bytesConsumed)
                success &o (MaterializerFunc.get result)
              | false -> error &ctx tag Unmatch }
    )

  /// Tries to parse a byte in the `format` from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Bytef (format: NumberFormat) =
    utf8ValueParser<Utf8Parser, byte> "utf8Byte" format

  /// Tries to parse an int16 in the `format` from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Int16f (format: NumberFormat) =
    utf8ValueParser<Utf8Parser, int16> "utf8Int16" format

  /// Tries to parse an uint16 in the `format`  from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8UInt16f (format: NumberFormat) =
    utf8ValueParser<Utf8Parser, uint16> "utf8UInt16" format

  /// Tries to parse an int32  in the `format` from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Int32f (format: NumberFormat) =
    utf8ValueParser<Utf8Parser, int> "utf8Int32" format

  /// Tries to parse an uint32 in the `format`  from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8UInt32f (format: NumberFormat) =
    utf8ValueParser<Utf8Parser, uint32> "utf8UInt32" format

  /// Tries to parse an int64 in the `format`  from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Int64f (format: NumberFormat) =
    utf8ValueParser<Utf8Parser, int64> "utf8Int64" format

  /// Tries to parse an uint64 in the `format`  from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8UInt64f (format: NumberFormat) =
    utf8ValueParser<Utf8Parser, uint64> "utf8UInt64" format

  /// Tries to parse a byte from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method with the default ('g') format.
  /// To use another format, use the `utf8Int32f` function instead.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Byte () = utf8Bytef NumberFormat.Default

  /// Tries to parse an uint16 from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method with the default ('g') format.
  /// To use another format, use the `utf8UInt16f` function instead.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8UInt16 () = utf8UInt16f NumberFormat.Default

  /// Tries to parse an int16 from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method with the default ('g') format.
  /// To use another format, use the `utf8Int16f` function instead.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Int16 () = utf8Int16f NumberFormat.Default

  /// Tries to parse an int32 from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method with the default ('g') format.
  /// To use another format, use the `utf8Int32f` function instead.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Int32 () = utf8Int32f NumberFormat.Default

  /// Tries to parse an uint32 from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method with the default ('g') format.
  /// To use another format, use the `utf8UInt32f` function instead.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8UInt32 () = utf8UInt32f NumberFormat.Default

  /// Tries to parse an int64 from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method with the default ('g') format.
  /// To use another format, use the `utf8Int64f` function instead.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Int64 () = utf8Int64f NumberFormat.Default

  /// Tries to parse an uint64 from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method with the default ('g') format.
  /// To use another format, use the `utf8UInt64f` function instead.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8UInt64 () = utf8UInt64f NumberFormat.Default

  /// Tries to parse a bool from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Bool () =
    utf8ValueParser<Utf8Parser, bool> "utf8Bool" NumberFormat.Default

  /// Tries to parse a DateTime from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8DateTime () =
    utf8ValueParser<Utf8Parser, DateTime> "utf8DateTime" NumberFormat.Default

  /// Tries to parse a DateTimeOffset from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8DateTimeOffset () =
    utf8ValueParser<Utf8Parser, DateTimeOffset> "utf8DateTimeOffset" NumberFormat.Default

  /// Tries to parse a decimal from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Decimal () =
    utf8ValueParser<Utf8Parser, decimal> "utf8Decimal" NumberFormat.Default

  /// Tries to parse a double from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Double () =
    utf8ValueParser<Utf8Parser, double> "utf8Double" NumberFormat.Default

  /// Tries to parse an guid from the sequence, by utilizing the `System.Buffers.Text.Utf8Parser.TryParse` method.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let inline utf8Guid () =
    utf8ValueParser<Utf8Parser, Guid> "utf8Guid" NumberFormat.Default

  /// Tries to parse a Rune from the sequence, by utilizing the `System.Text.Rune.DecodeFromUtf8`.
  /// NOTE: This parser allocates and perform the conversion already in the parse phase.
  let utf8Rune () =
    Parser(
      ParserTag "utf8Rune",
      { new IParserFunc<Rune> with
          member __.Parse(ctx, tag, o) =
            if ctx.Reader.Remaining < 1 then
              endOfSequenceError &ctx tag
            else
              let mutable result = Unchecked.defaultof<Rune>
              let mutable bytesConsumed = Unchecked.defaultof<int>

              let span = getSpanFromAvailable &ctx.Reader 4

              match Rune.DecodeFromUtf8(span, &result, &bytesConsumed) with
              | OperationStatus.Done ->
                ctx.Reader.Advance(bytesConsumed)
                success &o (MaterializerFunc.get result)
              | OperationStatus.InvalidData -> error &ctx tag Unmatch
              | OperationStatus.NeedMoreData -> endOfSequenceError &ctx tag
              | _ -> error &ctx tag (Unknown("Unexpected error occured when parsing rune")) }
    )

  /// `trimStart` is equivalent of `(whileConsuming (utf8CharEq trimChar) .> utf8String ())` but instead of matching on `trimChar`
  /// the `trimChar` are removed when materializing.
  let inline utf8StringTrimStart (trimChar: char) =
    let trim = byte trimChar

    { new IMaterializerConverter<string> with
        override __.Materialize(span) =
          let trimmed = span.TrimStart(trim)
          Materialize.Helpers.convert trimmed }
    |> Parsers.consume

  /// `utf8StringTrim` trims the supplied char when materializing.
  let inline utf8StringTrim (trimChar: char) =
    let trim = byte trimChar

    { new IMaterializerConverter<string> with
        override __.Materialize(span) =
          let trimmed = span.Trim(trim)
          Materialize.Helpers.convert trimmed }
    |> Parsers.consume

  /// `utf8StringTrimEnd` trims all occurences of the supplied char from the end.
  let inline utf8StringTrimEnd (trimChar: char) =
    let trim = byte trimChar

    { new IMaterializerConverter<string> with
        override __.Materialize(span) =
          let trimmed = span.TrimEnd(trim)
          Materialize.Helpers.convert trimmed }
    |> Parsers.consume
