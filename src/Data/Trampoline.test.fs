module Fable.Effect.Test.Data.Trampoline

open Fable.Effect.Data
open Expect
open WebTestRunner

module Testing = 
  let recursionFn n = 
    let rec loop (i: int) =
      if i <= 0 then
        Trampoline.Finish i
      else
        Trampoline.Continue(fun () -> i-1 |> loop)
    loop n 


describe "Trampoline" <| fun () ->
  describe "when Finish" <| fun () ->
    it "return Finish value directly" <| fun () -> promise {
      let trampoline = Trampoline.Finish 0
      let result = Trampoline.run trampoline
      Expect.equal 0 result
    }

  describe "when Continue" <| fun () ->
    it "return Finish value" <| fun () -> promise {
      let trampoline =  (fun () -> Trampoline.Finish (3 + 4)) |> Trampoline.Continue
      let result = Trampoline.run trampoline
      Expect.equal 7 result
    }

  describe "when deep recursion" <| fun () ->
    it "return to 0" <| fun () -> promise {
      let trampoline =  Testing.recursionFn 100000000
      let result = Trampoline.run trampoline
      Expect.equal 0 result
    }

