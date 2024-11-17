namespace Fable.Effect.Async

open Fable.Core.JS
open Fable.Effect.Data

type AsyncF<'a> = Free<Promise<'a>, 'a>

module Async =
  let resolve = Constructors.Promise.resolve
  let interpretWithFold (program: AsyncF<'a>): Promise<'a> =
    Free.fold
      resolve
      (fun p k -> p.``then``(fun () -> k()))
      program
