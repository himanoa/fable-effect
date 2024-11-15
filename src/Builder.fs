module Fable.Effect.Builder

open Fable.Effect.Operators

type MonadBuilder() = 
  member inline _.Return(value) = purify value
  member inline _.ReturnFrom(value) = purify value
  member inline _.Bind(value, binder) = value >>= binder
  member inline _.Discard(_value) = purify ()

let monad = new MonadBuilder()

