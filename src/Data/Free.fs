namespace Fable.Effect.Data

open Fable.Core.JS
open Fable.Effect.Control


[<NoComparison>]
type Free<'f, 'a> = 
  | PureValue of 'a
  | Free of Promise<Free<'f, 'a>>
  static member Pure (
    x: 'a,
    [<OptionalArgument>] _output: Free<'f, 'a>,
    [<OptionalArgument>] _mthd: Purifiable
  ) =
    PureValue x
  static member Map (source: Free<'f, 'a>, mapping: 'a -> 'b, [<OptionalArgument>] _mthd: Functor): Free<'f, 'b> = 
    let rec map(mapping: 'a -> 'b) (source: Free<'f, 'a>) : Free<'f, 'b> =  
      match source with
      | PureValue s ->
        PureValue (mapping s)
      | Free t -> 
        let maped = t.``then`` (fun x ->
          map mapping x
        )
        Free.Free(maped)
    map mapping source
  // static member Bind (value: Free<'f, 'a>, fn: 'a -> Free<'f, 'b>, [<OptionalArgument>] _mthd: Binder): Free<'f, 'b> = 
  //   match value with
  //   | PureValue a -> fn a
  //   | Free t ->
  //     (t |> unbox<Promise<obj>>).``then`` (fun x -> x |> unbox<'a> |> fn) |> unbox |> Free.Free
  static member Bind (value: Free<'f, 'a>, fn: 'a -> Free<'f, 'b>, [<OptionalArgument>] _mthd: Binder): Free<'f, 'b> = 
    let rec bind (f: 'a -> Free<'f, 'b>) (value: Free<'f, 'a>): Free<'f, 'b> = 
      match value with
      | PureValue s -> f s
      | Free t -> 
        let binded = t.``then`` (fun x ->
          bind fn x
        )
        Free.Free binded
    bind fn value

module Free = 
  let liftF(value: 'f): Free<'f, 'a> = Free.Free(Fable.Core.JS.Constructors.Promise.resolve (Free.Free value))

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

// module Testing = 
//   type Effect<'a> =
//     | Log of string * (unit -> 'a) // ログ出力
// 
//   type EffectF<'a> = Free<Effect<'a>, 'a>
// 
//   let log (message: string): EffectF<unit> =
//     let effect = Effect.Log(message, fun () -> ())
//     Free.liftF(effect)
// 
//   let rec interpret (program: Free<Effect<'a>, 'a>): Fable.Core.JS.Promise<'a> =
//     match program with
//     | PureValue x -> Fable.Core.JS.Constructors.Promise.resolve x
//     | Free pr -> promise {
//       let! effect = pr.``then`` unbox<EffectF<'a>>
//       match effect with
//       | Log (message, next) ->
//         printfn "Log: %s" message
//         return! interpret (next ())
//     }

