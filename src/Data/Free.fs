namespace Fable.Effect.Data

open Fable.Effect.Control

[<NoComparison>]
type Free<'f, 'a> = 
  | PureValue of 'a
  | Free of 'f * (unit -> Free<'f, 'a>)
  static member Pure (
    x: 'a,
    [<OptionalArgument>] _output: Free<'f, 'a>,
    [<OptionalArgument>] _mthd: Purifiable
  ) =
    PureValue x
  static member Map (source: Free<'f, 'a>, mapping: 'a -> 'b, [<OptionalArgument>] _mthd: Functor): Free<'f, 'b> = 
    let rec map (free: Free<'f, 'a>): Free<'f, 'b> = 
      match free with
      | PureValue s -> PureValue(mapping s)
      | Free (fx, k) ->  Free(fx, fun () -> map (k()))
    map source

  static member Bind (value: Free<'f, 'a>, fn: 'a -> Free<'f, 'b>, [<OptionalArgument>] _mthd: Binder): Free<'f, 'b> = 
    let rec bind(free: Free<'f,'a>): Trampoline<Free<'f, 'b>> =
      match free with
      | PureValue x -> Finish(fn x)
      | Free(fx, k) -> Continue(fun () ->
        let binded = bind (k())
        Functor.Invoke (fun next -> Free(fx, fun () -> next)) binded
      )
    Trampoline.run (bind value)
    

module Free = 
  let liftF(value: 'f): Free<'f, unit> = Free(value, fun () ->  PureValue ())

  let rec foldT (onPureValue: 'a -> Trampoline<'r>) (onFree: 'f -> (unit -> Free<'f, 'a>) -> Trampoline<'r>) (program: Free<'f, 'a>): Trampoline<'r>  = 
    match program with
    | PureValue x -> onPureValue x
    | Free (fx, k) ->  onFree fx k

  let rec run (program: Free<'f, 'a>): 'a = 
    let rec step (fx: 'f) (k: unit -> Free<'f, 'a>): Trampoline<'a> = 
      Continue(fun () -> foldT Finish step (k()))
    Trampoline.run (foldT Finish step program)

  let makeInterpreter (handler: 'fx -> Trampoline<unit>) (program: Free<'fx, 'a>): Free<'g, 'a> =
    let rec interpret (program: Free<'fx, 'a>): Trampoline<Free<'g, 'a>> =
      match program with
      | PureValue x -> Finish(PureValue x)
      | Free(fx, k) -> 
        Continue(fun () ->
          let trampoline  = handler fx
          let fn = fun next -> interpret (k next)
          Binder.Invoke trampoline fn
        )
    Trampoline.run (interpret program)
