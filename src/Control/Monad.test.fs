module Fable.Effect.Test.Control.Monad

open Fable.Effect.Control
open Expect
open WebTestRunner

describe "Monad" <| fun () ->

  describe "#Bind" <| fun () ->

    describe "when list" <| fun () ->
      it "binds a function over a list" <| fun () -> promise {
        let actual = Binder.Invoke [1; 2; 3] (fun x -> [x * 2])
        Expect.equal actual [2; 4; 6]
      }

      it "returns an empty list when input is empty" <| fun () -> promise {
        let actual = Binder.Invoke [] (fun x -> [x * 2])
        Expect.equal actual []
      }

    describe "when option" <| fun () ->
      it "binds a function over Some" <| fun () -> promise {
        let actual = Binder.Invoke (Some 10) (fun x -> Some (x * 2))
        Expect.equal actual (Some 20)
      }

      it "returns None when input is None" <| fun () -> promise {
        let actual = Binder.Invoke None (fun x -> Some (x * 2))
        Expect.equal actual None
      }

    describe "when result" <| fun () ->
      it "binds a function over Ok" <| fun () -> promise {
        let actual = Binder.Invoke (Ok 10) (fun x -> Ok (x * 2))
        Expect.equal actual (Ok 20)
      }

      it "returns the first Error when input is Error" <| fun () -> promise {
        let actual = Binder.Invoke (Error "Some error") (fun x -> Ok (x * 2))
        Expect.equal actual (Error "Some error")
      }

describe "Monad laws" <| fun () ->

  describe "Left identity" <| fun () ->
    it "satisfies left identity for list" <| fun () -> promise {
      let returnM = fun x -> [x]
      let f = fun x -> [x * 2]
      let actual = Binder.Invoke (returnM 10) f
      let expected = f 10
      Expect.equal actual expected
    }

    it "satisfies left identity for option" <| fun () -> promise {
      let returnM = Some
      let f = fun x -> Some (x * 2)
      let actual = Binder.Invoke (returnM 10) f
      let expected = f 10
      Expect.equal actual expected
    }

    it "satisfies left identity for result" <| fun () -> promise {
      let returnM = Ok
      let f = fun x -> Ok (x * 2)
      let actual = Binder.Invoke (returnM 10) f
      let expected = f 10
      Expect.equal actual expected
    }

  describe "Right identity" <| fun () ->
    it "satisfies right identity for list" <| fun () -> promise {
      let m = [1; 2; 3]
      let returnM = fun x -> [x]
      let actual = Binder.Invoke m returnM
      Expect.equal actual m
    }

    it "satisfies right identity for option" <| fun () -> promise {
      let m = Some 10
      let returnM = Some
      let actual = Binder.Invoke m returnM
      Expect.equal actual m
    }

    it "satisfies right identity for result" <| fun () -> promise {
      let m = Ok 10
      let returnM = Ok
      let actual = Binder.Invoke m returnM
      Expect.equal actual m
    }

  describe "Associativity" <| fun () ->
    it "satisfies associativity for list" <| fun () -> promise {
      let m = [1; 2; 3]
      let f = fun x -> [x * 2]
      let g = fun x -> [x + 1]
      let left = Binder.Invoke (Binder.Invoke m f) g
      let right = Binder.Invoke m (fun x -> Binder.Invoke (f x) g)
      Expect.equal left right
    }

    it "satisfies associativity for option" <| fun () -> promise {
      let m = Some 10
      let f = fun x -> Some (x * 2)
      let g = fun x -> Some (x + 1)
      let left = Binder.Invoke (Binder.Invoke m f) g
      let right = Binder.Invoke m (fun x -> Binder.Invoke (f x) g)
      Expect.equal left right
    }

    it "satisfies associativity for result" <| fun () -> promise {
      let m = Ok 10
      let f = fun x -> Ok (x * 2)
      let g = fun x -> Ok (x + 1)
      let left = Binder.Invoke (Binder.Invoke m f) g
      let right = Binder.Invoke m (fun x -> Binder.Invoke (f x) g)
      Expect.equal left right
    }
