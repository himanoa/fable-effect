namespace Fable.Effect.Data

open Fable.Core.JS
open Fable.Effect.Control


[<NoComparison>]
type Free<'f, 'a> = 
  | PureValue of 'a
  | Free of Promise<obj>
  static member Pure (
    x: 'a,
    [<OptionalArgument>] _output: Free<'f, 'a>,
    [<OptionalArgument>] _mthd: Purifiable
  ) =
    PureValue x
  static member Map (source: Free<'f, 'a>, mapping: 'a -> 'b, [<OptionalArgument>] _mthd: Functor): Free<'f, 'b> = 
    match source with
    | PureValue s ->
      PureValue (mapping s)
    | Free t ->
      t.``then`` (fun x ->  x |> unbox<'a> |> mapping |>  box) |> box |> unbox |> Free
  static member Bind (value: Free<'f, 'a>, fn: 'a -> Free<'f, 'b>, [<OptionalArgument>] _mthd: Binder): Free<'f, 'b> = 
    let tap (action: 'T -> unit) (x: 'T) : 'T =
      action x
      x
    match value with
    | PureValue a -> fn a
    | Free t ->
      (t |> unbox<Promise<obj>>).``then`` (fun x -> x |> unbox<'a> |> fn) |> unbox |> Free.Free

module Free = 
  let liftF(value: 'a): Free<'f, 'a> = Free.Free(Fable.Core.JS.Constructors.Promise.resolve (value |> box |> unbox))

  let rec run(program: Free<'f, 'a>): Fable.Core.JS.Promise<'a> = 
    let rec exec(program: Free<'f, 'a>) = 
      match program with
      | Free.PureValue x -> Fable.Core.JS.Constructors.Promise.resolve x
      (*
        NOTE: execの後のbox unboxで型検査を破壊してコンパイルを通している。
              外すと execがFree<'a> -> Promise<'a> なため、このパターンの戻り値が Promise<Promise<'a>>に推論されて壊れてしまう。
              JSのPromiseにはPromise<Promise<T>>を Promise<T>にflattenしてくれる機能があるため実用上無理なキャストをしても問題はない
      *)
      | Free.Free promise -> (promise |> unbox<Promise<obj>>).``then`` (fun next -> next |> unbox<Free<'f, 'a>> |> exec |> box |> unbox<'a>)
    exec program
