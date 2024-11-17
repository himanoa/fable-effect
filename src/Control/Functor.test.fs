module Fable.Effect.Test.Control.Functor

open Fable.Effect.Control
open Expect
open WebTestRunner

describe "Functor"
<| fun () ->

    describe "#Invoke"
    <| fun () ->

        describe "when list"
        <| fun () ->
            it "maps a function over a list"
            <| fun () ->
                promise {
                    let actual = Functor.Invoke (fun x -> x * 2) [ 1; 2; 3 ]
                    Expect.equal actual [ 2; 4; 6 ]
                }

            it "returns an empty list when input is empty"
            <| fun () ->
                promise {
                    let actual = Functor.Invoke (fun x -> x * 2) []
                    Expect.equal actual []
                }

        describe "when option"
        <| fun () ->
            it "maps a function over Some"
            <| fun () ->
                promise {
                    let actual = Functor.Invoke (fun x -> x * 2) (Some 10)
                    Expect.equal actual (Some 20)
                }

            it "returns None when input is None"
            <| fun () ->
                promise {
                    let actual = Functor.Invoke (fun x -> x * 2) None
                    Expect.equal actual None
                }

        describe "when result"
        <| fun () ->
            it "maps a function over Ok"
            <| fun () ->
                promise {
                    let actual = Functor.Invoke (fun x -> x * 2) (Ok 10: Result<int, string>)
                    Expect.equal actual (Ok 20)
                }

            it "returns Error unchanged when input is Error"
            <| fun () ->
                promise {
                    let actual =
                        Functor.Invoke (fun x -> x * 2) (Error "Error message": Result<int, string>)

                    Expect.equal actual (Error "Error message")
                }

        describe "when function"
        <| fun () ->
            it "composes a function with the mapping"
            <| fun () ->
                promise {
                    let original = (fun x -> x * 2)
                    let actual = Functor.Invoke (fun x -> x + 1) original
                    let result = actual 5
                    Expect.equal result 11
                }
