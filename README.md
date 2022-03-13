# Pansar

**Pansar** aims to be a high performance, low memory allocation F# parser combinator library. The library is built upon the existing robust, high-speed components of .NET, such as `Span<>`, `ReadOnlySequence<>` and `SequenceReader<>`. Most of these high-speed components resides on the stack and guides one to a more imperative programming style. Pansar tries to encapsulate all imperative parts and instead package it in a more functional style API, without compromising (too much) speed.

NOTE: The library is in an early development phase and not production ready.

## Example

### Define a parser
```fsharp
open Pansar
open Pansar.Text
open Pansar.Text.Parsers

type Color = { Red: byte; Green: byte; Blue: byte }

let parser () =

  let numberSign = Match.char '#' /> Materialize.ignore
  let channel = leftSlice 2 (utf8Bytef NumberFormat.Hexadecimal)

  // Match on '#' but discard the char from the result, and then match three channels and keep their results.
  numberSign .> consume3 channel
  |>> (fun ((r, g), b) -> {
    Red = r;
    Green = g;
    Blue = b })
```

### Run parser

```fsharp
let result =

  // 1. Select what should be parsed
  From.string "#ff6600"

  // 2. Run the parser on the source using one of the available strategies
  |> Parse.withException (parser ())

printf $"R=%i{result.Red} G=%i{result.Green} B=%i{result.Blue}"

// Prints:
// R=255 G=102 B=0
```

More examples can be found in the [samples](samples/README.md) folder.

## License

[Apache 2.0](https://raw.githubusercontent.com/ljungloef/Pansar/main/LICENSE)