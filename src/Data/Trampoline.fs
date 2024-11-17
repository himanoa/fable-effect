namespace Fable.Effect.Data

open Fable.Effect.Control

type Trampoline<'a> =
    | Finish of 'a
    | Continue of (unit -> Trampoline<'a>)

    static member Pure(x: 'a, [<OptionalArgument>] _output: Trampoline<'a>, [<OptionalArgument>] _mthd: Purifiable) =
        Finish x

    static member Map(source: Trampoline<'a>, mapping: 'a -> 'b, [<OptionalArgument>] _mthd: Functor) : Trampoline<'b> =
        let rec map f =
            function
            | Finish x -> Finish(f x)
            | Continue g -> Continue(fun () -> map f (g ()))

        map mapping source

    static member Bind
        (value: Trampoline<'a>, fn: 'a -> Trampoline<'b>, [<OptionalArgument>] _mthd: Binder)
        : Trampoline<'b> =
        let rec bind f =
            function
            | Finish x -> f x
            | Continue g -> Continue(fun () -> bind f (g ()))

        bind fn value

module Trampoline =
    let rec run (program: Trampoline<'a>) =
        match program with
        | Finish x -> x
        | Continue f -> f () |> run
