module Fable.Effect.Test.Control.Applicative

open Fable.Effect.Control
open Expect 
open WebTestRunner

describe "Applicative" <| fun () ->
  describe "#Invoke" <| fun () -> 
    describe "when list" <| fun () -> 
      it "return to 13" <| fun () ->  promise {
        let actual =  [10] |> Appliable.Invoke [(fun x -> x + 1); (fun x -> x + 2)]
        Expect.equal actual [13]
      }
