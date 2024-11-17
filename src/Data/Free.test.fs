module Fable.Effect.Test.Data.FreeMonad

open Fable.Effect.Data
open Expect
open WebTestRunner
open Fable.Effect.Control

type TestEffect =
  | Count of int
  | Log of string

describe "Free Monad" <| fun () ->
  describe "PureValue" <| fun () ->
    it "creates a PureValue holding a value" <| fun () -> promise {
      let value = 42
      let p = Free.Pure(value)
      match p with
      | PureValue v -> Expect.equal v value
      | _ -> failwith "Expected PureValue"
    }

  describe "Free" <| fun () ->
    it "creates a Free from an effect and continuation" <| fun () -> promise {
      let effect = Count 42
      let free = Free.liftF effect
      match free with
      | Free(Count v, k) -> 
        let next = k()
        match next with
        | PureValue () -> Expect.equal v 42
        | _ -> failwith "Expected PureValue unit as continuation"
      | _ -> failwith "Expected Free"
    }

  describe "#Map" <| fun () ->
    it "maps a function over a PureValue" <| fun () -> promise {
      let p = PureValue 42
      let mapped = Functor.Invoke (fun x -> x * 2) p
      match mapped with
      | PureValue v -> Expect.equal v 84
      | _ -> failwith "Expected PureValue"
    }

    it "maps a function over a Free" <| fun () -> promise {
      let effect = Count 42
      let free = Free.liftF effect
      let mapped = Functor.Invoke (fun () -> 84) free
      match mapped with
      | Free(Count _, k) -> 
        let next = k()
        match next with
        | PureValue v -> Expect.equal v 84
        | _ -> failwith "Expected PureValue"
      | _ -> failwith "Expected Free"
    }

    it "handles nested maps without stack overflow" <| fun () -> promise {
      let initialValue = PureValue 1
      let depth = 10000
      let result = 
        [1..depth] 
        |> List.fold (fun acc _ -> 
          Functor.Invoke ((+) 1) acc) initialValue
      
      match result with
      | PureValue v -> Expect.equal v (depth + 1)
      | _ -> failwith "Expected PureValue"
    }

  describe "#Bind" <| fun () ->
    it "binds a function to a PureValue" <| fun () -> promise {
      let p = PureValue 42
      let bound = Binder.Invoke p (fun x -> PureValue(x * 2))
      match bound with
      | PureValue v -> Expect.equal v 84
      | _ -> failwith "Expected PureValue"
    }

    it "binds a function to a Free" <| fun () -> promise {
      let effect = Count 42
      let free = Free.liftF effect
      let bound = Binder.Invoke free (fun () -> PureValue 84)
      match bound with
      | Free(Count _, k) -> 
        let next = k()
        match next with
        | PureValue v -> Expect.equal v 84
        | _ -> failwith "Expected PureValue"
      | _ -> failwith "Expected Free"
    }

    it "handles nested binds without stack overflow" <| fun () -> promise {
      let initialValue = PureValue 1
      let depth = 10000
      let result = 
        [1..depth] 
        |> List.fold (fun acc _ -> 
          Binder.Invoke acc (fun x -> PureValue(x + 1))) initialValue
      
      match result with
      | PureValue v -> Expect.equal v (depth + 1)
      | _ -> failwith "Expected PureValue"
    }

  describe "computation with effects" <| fun () ->
    let rec interpret = function
      | PureValue x -> x
      | Free(Count n, k) -> interpret (k())
      | Free(Log msg, k) -> 
          interpret (k())

    it "composes multiple effects" <| fun () -> promise {
      let program = 
        Binder.Invoke (Free.liftF (Count 42)) (fun () ->
        Binder.Invoke (Free.liftF (Log "test")) (fun () ->
        PureValue 84))

      let result = interpret program
      Expect.equal result 84
    }
