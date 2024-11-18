namespace Himanoa.Fable.Effect.Control


type Appliable =

  static member inline Apply
    (fnOpt: Option<'a -> 'b>, valueOpt: Option<'a>, [<OptionalArgument>] _mthd: Appliable)
    : Option<'b> =
    match fnOpt, valueOpt with
    | Some fn, Some v -> Some(fn v)
    | _ -> None

  static member inline Apply
    (fnOpt: Result<'a -> 'b, 'e>, valueOpt: Result<'a, 'e>, [<OptionalArgument>] _mthd: Appliable)
    : Result<'b, 'e> =
    match fnOpt, valueOpt with
    | Ok fn, Ok v -> Ok(fn v)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  static member inline Apply
    (fnList: List<'a -> 'b>, valueList: List<'a>, [<OptionalArgument>] _mthd: Appliable)
    : List<'b> =
    List.collect (fun f -> List.map f valueList) fnList


  static member inline Invoke (f: '``Applicative<'T->'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
    let inline call (mthd: ^M, fn: ^F, input: ^I, _output: ^R) =
      ((^M or ^I or ^F or ^R): (static member Apply: _ * _ * _ -> _) fn, input, mthd)

    call (Unchecked.defaultof<Appliable>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)


type Purifiable =
  static member inline Pure
    (value: 'a, [<OptionalArgument>] _output: List<'a>, [<OptionalArgument>] _mthd: Purifiable)
    : List<'a> =
    [ value ]

  static member inline Pure
    (value: 'a, [<OptionalArgument>] _output: Option<'a>, [<OptionalArgument>] _mthd: Purifiable)
    : Option<'a> =
    Some value

  static member inline Pure
    (value: 'a, [<OptionalArgument>] _output: Result<'a, _>, [<OptionalArgument>] _mthd: Purifiable)
    : Result<'a, _> =
    Ok value

  static member inline Invoke(value: 'a) : 'b =
    let inline call (mthd: ^M, value: ^F, _output: ^R) : 'b =
      ((^M or ^R): (static member Pure: _ * _ * _ -> _) value, _output, mthd)

    call (Unchecked.defaultof<Purifiable>, value, Unchecked.defaultof<'b>)
