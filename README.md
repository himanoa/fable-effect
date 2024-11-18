# himanoa-fable-effect

An effect system implementation written in F# that can be compiled to JavaScript. This library controls side effects using monads, applicatives and functors.

## Installation

```bash
dotnet add package himanoa-fable-effect
```

## Usage Examples

### Free Monad

```fsharp
open Himanoa.Fable.Effect.Operators

type TestEffect<'a> =
  | Count of int * ('a -> 'a)
  | Log of string * (unit -> 'a)

type TestEffectF =
  interface HKT
  static member Assign(_: App<TestEffectF, 'a>, _: TestEffect<'a>) : unit = ()

module TestEffect =
  let wrap (fa: TestEffect<'a>) : App<TestEffectF, 'a> = HKT.pack fa
  let unwrap (app: App<TestEffectF, 'a>) = HKT.unpack app

// Smart constructors
let count (n) : Free<TestEffectF, _, 's> =
  Free.liftF (TestEffect.wrap (Count(n, id)))

let log (msg) : Free<TestEffectF, _, 's> =
  Free.liftF (TestEffect.wrap (Log(msg, id)))

// Program construction
let program: Free<TestEffectF, _, int> =
  monad {
    let! value = count 42
    do! log "Count completed"
    return value
  }

// Stack-safe interpreter implementation
let interpretProgram program =
  let interpreter (fx: App<TestEffectF, 'r>) (k: 'r -> 'a) : 'a =
    match TestEffect.unwrap fx with
    | Count(n, next) ->
      printfn "Count: %d" n
      k (next (n |> unbox<'r>))
    | Log(msg, next) ->
      printfn "Log: %s" msg
      k (next ())

  Free.fold interpreter id program

// Program execution
let result = interpretProgram program
```

### State Monad

```fsharp
open Himanoa.Fable.Effect.Data
open Himanoa.Fable.Effect.Builder

let stateProgram = monad {
    do! State.put 1
    let! value = State.get()
    do! State.put (value + 1)
    return value
}

let (finalState, result) = State.runState 0 stateProgram
```

## Features

- Type-level programming with Higher-Kinded Types (HKT)
- Stack overflow prevention with Trampoline
- F# to JavaScript compilation support
- Type-safe side effect control

## Development

```bash
# Install dependencies
dotnet tool restore
dotnet paket restore
pnpm install

# Run tests
pnpm test

# Run build
pnpm build
```

## License

MIT

## Contributing

Issues and Pull Requests are welcome. Before making major changes, please discuss them in an Issue first.

## Author

himanoa

---

For detailed usage and API documentation, please refer to the [documentation](docs/README.md).
