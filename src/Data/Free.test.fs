module Himanoa.Fable.Effect.Test.Data.FreeMonad

open Himanoa.Fable.Effect.Data
open Expect
open WebTestRunner
open Himanoa.Fable.Effect.Control
open Himanoa.Fable.Effect.Operators

// テスト用のエフェクト定義
type TestEffect<'a> =
  | Count of int * ('a -> 'a)
  | Log of string * ('a -> 'a)

type TestEffectF =
  interface HKT
  static member Assign(_: App<TestEffectF, 'a>, _: TestEffect<'a>) : unit = ()

module TestEffect =
  let wrap (fa: TestEffect<'a>) : App<TestEffectF, 'a> = HKT.pack fa
  let unwrap (app: App<TestEffectF, 'a>) : TestEffect<'a> = HKT.unpack app

let count n =
  Free.liftF (TestEffect.wrap (Count(n, id)))

let log msg =
  Free.liftF (TestEffect.wrap (Log(msg, id)))

describe "Free Monad"
<| fun () ->
  describe "PureValue"
  <| fun () ->
    it "creates a PureValue holding a value"
    <| fun () ->
      promise {
        let value = 42
        let p = purify value

        match p with
        | PureValue v -> Expect.equal v value
        | _ -> failwith "Expected PureValue"
      }

  describe "Free"
  <| fun () ->
    it "creates a Free from an effect and continuation"
    <| fun () ->
      promise {
        let free = count 42

        match free with
        | Free(fx, k) ->
          let action = TestEffect.unwrap fx

          match action with
          | Count(n, _) -> Expect.equal n 42
          | _ -> failwith "Expected Count effect"
        | _ -> failwith "Expected Free"
      }

  describe "#Map"
  <| fun () ->
    it "maps a function over a PureValue"
    <| fun () ->
      promise {
        let p = purify 42
        let mapped = Functor.Invoke (fun x -> x * 2) p

        match mapped with
        | PureValue v -> Expect.equal v 84
        | _ -> failwith "Expected PureValue"
      }

    it "maps a function over a Free"
    <| fun () ->
      promise {
        let free = count 42
        let mapped = Functor.Invoke (fun x -> x * 2) free

        match mapped with
        | Free(fx, k) ->
          let action = TestEffect.unwrap fx

          match action with
          | Count(n, _) -> Expect.equal n 42
          | _ -> failwith "Expected Count effect"
        | _ -> failwith "Expected Free"
      }

    it "handles nested maps without stack overflow"
    <| fun () ->
      promise {
        let initialValue = purify 1
        let depth = 10000

        let result =
          [ 1..depth ] |> List.fold (fun acc _ -> Functor.Invoke ((+) 1) acc) initialValue

        match result with
        | PureValue v -> Expect.equal v (depth + 1)
        | _ -> failwith "Expected PureValue"
      }

  describe "#Bind"
  <| fun () ->
    it "binds a function to a PureValue"
    <| fun () ->
      promise {
        let p = purify 42
        let bound = Binder.Invoke p (fun x -> purify (x * 2))

        match bound with
        | PureValue v -> Expect.equal v 84
        | _ -> failwith "Expected PureValue"
      }

    it "binds a function to a Free"
    <| fun () ->
      promise {
        let free = count 42
        let bound = Binder.Invoke free (fun _ -> purify 84)

        match bound with
        | Free(fx, _) ->
          let action = TestEffect.unwrap fx

          match action with
          | Count(n, _) -> Expect.equal n 42
          | _ -> failwith "Expected Count effect"
        | _ -> failwith "Expected Free"
      }

    it "handles nested binds without stack overflow"
    <| fun () ->
      promise {
        let initialValue = purify 1
        let depth = 10000

        let result =
          [ 1..depth ]
          |> List.fold (fun acc _ -> Binder.Invoke acc (fun x -> purify (x + 1))) initialValue

        match result with
        | PureValue v -> Expect.equal v (depth + 1)
        | _ -> failwith "Expected PureValue"
      }
