namespace Himanoa.Fable.Effect.Data.Async

open Himanoa.Fable.Effect.Data
open Himanoa.Fable.Effect.Builder
open Fable.Core.JS

type AsyncAction<'a> = RunPromise of Promise<'a>

type AsyncActionF =
  interface HKT
  static member Assign(_: App<AsyncActionF, 'a>, _: AsyncAction<'a>) : unit = ()

module AsyncAction =
  let wrap (fa: AsyncAction<'a>) : App<AsyncActionF, 'a> = HKT.pack fa
  let unwrap (app: App<AsyncActionF, 'a>) = HKT.unpack app


module Async =
  let fromPromise (p: Promise<'a>) : Free<AsyncActionF, 'a, 'a> =
    Free.liftF (AsyncAction.wrap (RunPromise p))

  let rec runAsync (program: Free<AsyncActionF, 'r, 'a>) : Promise<'a> =
    promise {
      match program with
      | PureValue x -> return x
      | Free(fx, k) ->
        match AsyncAction.unwrap fx with
        | RunPromise p ->
          let! result = p
          let! finalResult = runAsync (k result)
          return finalResult
    }
