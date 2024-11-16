module Fable.Effect.Test.Data.FreeMonad

open Fable.Effect.Data
open Fable.Core.JS
open Expect
open WebTestRunner
open Fable.Effect.Operators
open Fable.Effect.Control

describe "Free Monad" <| fun () ->

  describe "PureValue" <| fun () ->
    it "creates a PureValue holding a value" <| fun () -> promise {
      let value = 42
      let p = purify value
      match p with
      | PureValue v -> Expect.equal v value
      | _ -> failwith "Expected PureValue"
    }

  describe "Free" <| fun () ->
    it "creates a Free from a resolved promise" <| fun () -> promise {
      let promise = Fable.Core.JS.Constructors.Promise.resolve (purify 42)
      let free = Free.Free promise
      let! result = Free.run free
      Expect.equal result 42
    }

  describe "#Map" <| fun () ->
    it "maps a function over a PureValue" <| fun () -> promise {
      let p = Free.Pure(42)
      let mapped = Functor.Invoke (fun x -> x * 2) p
      match mapped with
      | PureValue v -> Expect.equal v 84
      | _ -> failwith "Expected PureValue"
    }

    it "maps a function over a Free" <| fun () -> promise {
      let promise = Fable.Core.JS.Constructors.Promise.resolve (purify 42)
      let free = Free.Free promise
      let mapped = Functor.Invoke (fun x -> x * 2) free
      let! result = Free.run mapped
      Expect.equal result 84
    }

  describe "#Bind" <| fun () ->
    it "binds a function to a PureValue" <| fun () -> promise {
      let p = Free.Pure(42)
      let bound = Binder.Invoke p (fun x -> Free.Pure(x * 2))
      match bound with
      | PureValue v -> Expect.equal v 84
      | _ -> failwith "Expected PureValue"
    }

    it "binds a function to a Free" <| fun () -> promise {
      let promise = Fable.Core.JS.Constructors.Promise.resolve (purify 42)
      let free = Free.Free promise
      let bound = Binder.Invoke free (fun x -> Free.Pure(x * 2))
      let! result = Free.run bound
      Expect.equal result 84
    }

  describe "#run" <| fun () ->
    it "runs a PureValue" <| fun () -> promise {
      let p = Free.Pure(42)
      let! result = Free.run p
      Expect.equal result 42
    }

    it "runs a Free with a resolved promise" <| fun () -> promise {
      let promise = Fable.Core.JS.Constructors.Promise.resolve (purify 42)
      let free = Free.Free promise
      let! result = Free.run free
      Expect.equal result 42
    }

    it "runs a nested Free" <| fun () -> promise {
      let promise = Fable.Core.JS.Constructors.Promise.resolve (Free.Free(Fable.Core.JS.Constructors.Promise.resolve (Free.Pure(42))))
      let free = Free.Free promise
      let! result = Free.run free
      Expect.equal result 42
    }
