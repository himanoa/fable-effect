namespace Fable.Effect.Control

type Binder = 
  static member inline Bind(
    value: 'a list,
    fn: 'a -> 'b list,
    [<OptionalArgument>] _mthd: Binder
  ): 'b list =
    List.collect fn value

  static member inline Bind(
    value: 'a option,
    fn: 'a -> 'b option,
    [<OptionalArgument>] _mthd: Binder
  ): 'b option =
    Option.bind fn value

  static member inline Bind(
    value: Result<'a, _>,
    fn: 'a -> Result<'b, _>,
    [<OptionalArgument>] _mthd: Binder
  ): Result<'b, _> =
    Result.bind fn value

  static member inline Invoke (value: 'a) (fn: 'b)  : 'c =
    let inline call (
      mthd: ^M,
      value: ^F,
      fn: ^I,
      _output: ^R
    ) =
      ((^M or ^I or ^F or ^R) : (static member Bind: _ * _ * _ -> _) value, fn, mthd)
    call (Unchecked.defaultof<Binder>, value, fn, Unchecked.defaultof<'c>)

module Testing = 
  let a(): int list =
    let b = [1;2;3]
    let c = fun x -> [x]
    Binder.Invoke b c 

