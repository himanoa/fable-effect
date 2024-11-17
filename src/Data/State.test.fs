module Fable.Effect.Test.Data.State

open Fable.Effect.Data
open Expect
open WebTestRunner
open Fable.Effect.Builder

module Testing =
  let simpleGetProgram<'s> () =
    monad {
      let! s = State.get<'s> ()
      return s
    }

  let simplePutProgram s =
    monad {
      do! State.put s
      return ()
    }

  let complexProgram (initialValue: int) =
    monad {
      do! State.put initialValue
      let! value = State.get ()
      do! State.put (value + 1)
      let! finalValue = State.get ()
      return finalValue
    }

describe "State"
<| fun () ->
  describe "when Get"
  <| fun () ->
    it "returns current state value"
    <| fun () ->
      promise {
        let initialState = 42
        let program = Testing.simpleGetProgram ()
        let (finalState, result) = State.runState initialState program
        Expect.equal finalState initialState
        Expect.equal result initialState
      }

  describe "when Put"
  <| fun () ->
    it "updates state value"
    <| fun () ->
      promise {
        let initialState = 0
        let newState = 100
        let program = Testing.simplePutProgram newState
        let (finalState, result) = State.runState initialState program
        Expect.equal finalState newState
        Expect.equal result ()
      }

  describe "when combining operations"
  <| fun () ->
    it "performs sequence of operations correctly"
    <| fun () ->
      promise {
        let initialState = 41
        let program = Testing.complexProgram initialState
        let (finalState, result) = State.runState initialState program
        Expect.equal finalState (initialState + 1)
        Expect.equal result (initialState + 1)
      }

  describe "when running multiple operations"
  <| fun () ->
    it "maintains state between operations"
    <| fun () ->
      promise {
        let program =
          monad {
            do! State.put 1
            let! v1 = State.get ()
            do! State.put (v1 + 2)
            let! v2 = State.get ()
            do! State.put (v2 * 2)
            let! final = State.get ()
            return final
          }

        let (finalState, result) = State.runState 0 program
        Expect.equal finalState 6
        Expect.equal result 6
      }
