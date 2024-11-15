module Fable.Effect.Operators

open Fable.Effect.Control

let inline (>>=) (value: 'a) (fn: 'b): 'c =  Binder.Invoke value fn
let inline (=<<) (fn: 'b) (value: 'a): 'c =  Binder.Invoke value fn
let inline (<*>) (value: 'a) (fn: 'b): 'c =  Appliable.Invoke value fn
let inline (|>>) (value) (fn): 'c =  Functor.Invoke fn value
let inline (|<<) (fn)(value): 'c =  Functor.Invoke fn value

let inline purify (value: 'a): 'c =  Purifiable.Invoke value
let inline map(mapping, source) = Functor.Invoke mapping source

module Testing = 
  let bindOption() =
    let addOne = fun x -> Some (x + 1)
    let addTwo = fun x -> Some (x + 2)
    purify 12 >>= addOne >>= addTwo >>= addOne >>= addTwo >>= addOne >>= addTwo >>= addTwo

  let applyOption() = 
    let none: int option = None
    let some: int option = Some 1
    let add: int -> int -> int = fun x y -> x + y

    purify add <*> some <*> none
