namespace Fable.Effect.Data

open Fable.Core.JS
open Fable.Effect.Control

type Free<'a> = 
  | PureValue of 'a
  | Free of Promise<obj>
  static member Pure (
    x: 'a,
    [<OptionalArgument>] _output: Free<'a>,
    [<OptionalArgument>] _mthd: Purifiable
  ) =
    PureValue x
  static member Map (source: Free<'a>, mapping: 'a -> 'b, [<OptionalArgument>] _mthd: Functor): Free<'b> = 
    match source with
    | PureValue s -> PureValue (mapping s)
    | Free t -> t.``then`` (fun x ->  x |> unbox<Free<'a>> |> Functor.Invoke mapping |> box) |> Free
  static member Bind (value: Free<'a>, fn: 'a -> Free<'b>, [<OptionalArgument>] _mthd: Binder): Free<'b> = 
    match value with
    | PureValue a -> fn a
    | Free t -> t.``then`` (fun x -> x |> unbox<Free<'a>> |> Functor.Invoke fn |> box) |> Free

module Free = 
  let liftF(value: 'a) = Free.Free(value)

  let rec run(program: Free<'a>): Fable.Core.JS.Promise<'a> = 
    let rec exec(program: Free<'a>) = 
      match program with
      | PureValue x -> Fable.Core.JS.Constructors.Promise.resolve x
      (*
        NOTE: execの後のbox unboxで型検査を破壊してコンパイルを通している。
              外すと execがFree<'a> -> Promise<'a> なため、このパターンの戻り値が Promise<Promise<'a>>に推論されて壊れてしまう。
              JSのPromiseにはPromise<Promise<T>>を Promise<T>にflattenしてくれる機能があるため実用上無理なキャストをしても問題はない
      *)
      | Free promise -> promise.``then`` (fun next -> next |> unbox<Free<'a>> |> exec |> box |> unbox<'a>)
    exec program
