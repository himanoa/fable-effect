module Fable.Effect.Test.Data.Async

open Expect
open Fable.Core.JS
open WebTestRunner
open Fable.Effect.Data.Async
open Fable.Effect.Builder

describe "Async" <| fun () -> 
  describe "runAsync" <| fun () ->
    it "return to 43" <| fun () -> promise {
      let program = monad {
        let! foo = Async.fromPromise(Constructors.Promise.resolve 42)
        return foo + 1
      }
      let! result = Async.runAsync program
      Expect.equal result 43
    }

