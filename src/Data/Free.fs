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


(*
module Testing = 
  type Effect<'a> =
    | Log of string * (unit -> 'a) // ログ出力

  let log (message: string) =
    let effect = Effect.Log(message, fun () -> ())
    Free.liftF(effect)

  let rec interpret (program: Free<Effect<'a>, 'a>): Fable.Core.JS.Promise<'a> = promise {
    match program with
    | PureValue x -> return x
    | Free prom ->
      let! inner = prom
      match inner with
      | PureValue x -> 
        return x
      | Effect.Log (message, next) ->
        printfn "Log: %s" message
        return! interpret (next ())
      | Free _ as next ->
        return! interpret next
  }
*)
