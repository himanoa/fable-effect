module Fable.Effect.Test.Control.Applicative

open Fable.Effect.Control
open Expect
open WebTestRunner

describe "Applicative"
<| fun () ->

    describe "#Invoke"
    <| fun () ->

        describe "when list"
        <| fun () ->
            it "applies functions to list of values"
            <| fun () ->
                promise {
                    let actual = [ 10 ] |> Appliable.Invoke [ (fun x -> x + 1); (fun x -> x + 2) ]
                    Expect.equal actual [ 11; 12 ]
                }

            it "applies multiple functions to multiple values"
            <| fun () ->
                promise {
                    let actual = [ 1; 2 ] |> Appliable.Invoke [ (fun x -> x + 1); (fun x -> x * 2) ]
                    Expect.equal actual [ 2; 3; 2; 4 ]
                }

            it "returns an empty list when either list is empty"
            <| fun () ->
                promise {
                    let actual1 = [] |> Appliable.Invoke [ (fun x -> x + 1) ]
                    let actual2 = [ 1; 2 ] |> Appliable.Invoke []
                    Expect.equal actual1 []
                    Expect.equal actual2 []
                }

        describe "when option"
        <| fun () ->
            it "applies a function to a value when both are Some"
            <| fun () ->
                promise {
                    let actual = Some 10 |> Appliable.Invoke(Some(fun x -> x + 1))
                    Expect.equal actual (Some 11)
                }

            it "returns None when either is None"
            <| fun () ->
                promise {
                    let actual1 = None |> Appliable.Invoke(Some(fun x -> x + 1))
                    let actual2 = Some 10 |> Appliable.Invoke None
                    Expect.equal actual1 None
                    Expect.equal actual2 None
                }

        describe "when result"
        <| fun () ->
            it "applies a function to a value when both are Ok"
            <| fun () ->
                promise {
                    let actual = Ok 10 |> Appliable.Invoke(Ok(fun x -> x + 1))
                    Expect.equal actual (Ok 11)
                }

            it "returns the first Error when the function is Error"
            <| fun () ->
                promise {
                    let actual = Ok 10 |> Appliable.Invoke(Error "Function error")
                    Expect.equal actual (Error "Function error")
                }

            it "returns the second Error when the value is Error"
            <| fun () ->
                promise {
                    let actual = Error "Value error" |> Appliable.Invoke(Ok(fun x -> x + 1))
                    Expect.equal actual (Error "Value error")
                }

    describe "#Pure"
    <| fun () ->

        describe "when list"
        <| fun () ->
            it "creates a singleton list from a value"
            <| fun () ->
                promise {
                    let actual = Purifiable.Invoke 10: int list
                    Expect.equal actual [ 10 ]
                }

        describe "when option"
        <| fun () ->
            it "creates a Some value from a value"
            <| fun () ->
                promise {
                    let actual = Purifiable.Invoke 10: int option
                    Expect.equal actual (Some 10)
                }

        describe "when result"
        <| fun () ->
            it "creates an Ok value from a value"
            <| fun () ->
                promise {
                    let actual = Purifiable.Invoke 10: Result<int, string>
                    Expect.equal actual (Ok 10)
                }
