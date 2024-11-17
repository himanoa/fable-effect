namespace Fable.Effect.Data

open Fable.Effect.Control

[<NoComparison>]
type Free<'f, 'r, 'a when 'f :> HKT> =
  | PureValue of 'a
  | Free of App<'f, 'r> * ('r -> Free<'f, 'r, 'a>)

  static member Pure(x: 'a, [<OptionalArgument>] _output: Free<'f, 'r, 'a>, [<OptionalArgument>] _mthd: Purifiable) =
    PureValue x

  static member Map(source: Free<'f, 'r, 'a>, mapping: 'a -> 'b, [<OptionalArgument>] _mthd: Functor) : Free<'f, 'r, 'b> =
    let rec map (free: Free<'f, 'r, 'a>) : Free<'f, 'r, 'b> =
      match free with
      | PureValue s -> PureValue(mapping s)
      | Free(fx, k) -> Free(fx, fun r -> map (k r))

    map source

  static member Bind
    (value: Free<'f, 'r, 'a>, fn: 'a -> Free<'f, 'r, 'b>, [<OptionalArgument>] _mthd: Binder)
    : Free<'f, 'r, 'b> =
    let rec bind (free: Free<'f, 'r, 'a>) : Free<'f, 'r, 'b> =
      match free with
      | PureValue x -> fn x
      | Free(fx, k) -> Free(fx, fun r -> bind (k r))

    bind value


module Free =
  let liftF (value: App<'f, 'r>) : Free<'f, 'r, 'a> =
    Free(value, fun x -> x |> unbox<'a> |> PureValue)

  let rec run (handler: App<'f, 'r> -> 'r) (program: Free<'f, 'r, 'a>) : 'a =
    match program with
    | PureValue x -> x
    | Free(fx, k) ->
      let r = handler fx
      run handler (k r)

  let rec foldT
    (algebra: App<'f, 'b> -> ('b -> Trampoline<'r>) -> Trampoline<'r>)
    (pure': 'a -> Trampoline<'r>)
    (free: Free<'f, 'b, 'a>)
    : Trampoline<'r>
    =
    match free with
    | PureValue x -> pure' x
    | Free(fx, k) ->
      let fk = fun x -> Continue(fun () -> foldT algebra pure' (k x))
      algebra fx fk

  let rec fold (algebra: App<'f, 'b> -> ('b -> 'r) -> 'r) (pure': 'a -> 'r) (free: Free<'f, 'b, 'a>) : 'r =
    let algebraT fx fk =
      Continue(fun () -> Finish(algebra fx (fun x -> Trampoline.run (fk x))))

    let pureT x = Finish(pure' x)
    foldT algebraT pureT free |> Trampoline.run
