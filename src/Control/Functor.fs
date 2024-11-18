namespace Himanoa.Fable.Effect.Control

type Functor =
  static member Map(source: 'R -> 'T, mapping: 'T -> 'U, [<OptionalArgument>] _mthd: Functor) = source >> mapping

  static member Map(source: option<'T>, mapping: 'T -> 'U, [<OptionalArgument>] _mthd: Functor) =
    Option.map mapping source

  static member Map(source: Result<'T, 'E>, mapping: 'T -> 'U, [<OptionalArgument>] _mthd: Functor) =
    Result.map mapping source

  static member Map(source: list<'T>, mapping: 'T -> 'U, [<OptionalArgument>] _mthd: Functor) = List.map mapping source

  static member inline Invoke (mapping: 'T -> 'U) (source: '``Functor<T>``) : '``Functor<'U>`` =
    let inline call (mthd: ^M, source: ^I, _outut: ^R) =
      ((^M or ^I or ^R): (static member Map: _ * _ * _ -> _) source, mapping, mthd)

    call (Unchecked.defaultof<Functor>, source, Unchecked.defaultof<'``Functor<'U>``>)

[<AutoOpen>]
module FunctorImplicits =
  let a () =
    Functor.Invoke (fun x -> x + 1) [ 1; 2; 3 ]
