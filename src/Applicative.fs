namespace Fable.Control


type Applicative =

  static member inline Apply(
    fnOpt: Option<'a -> 'b>,
    valueOpt: Option<'a>,
    [<OptionalArgument>]_mthd: Applicative
  ): Option<'b> = 
    match fnOpt, valueOpt with
    | Some fn, Some v -> Some (fn v)
    | _ -> None

  static member inline Apply(
    fnOpt: Result<'a -> 'b, 'e>,
    valueOpt: Result<'a, 'e>,
    [<OptionalArgument>]_mthd: Applicative
  ): Result<'b, 'e> = 
    match fnOpt, valueOpt with
    | Ok fn, Ok v -> Ok (fn v)
    | Error e, _ -> Error e
    | _, Error e -> Error e

  static member inline Apply(
    fnList: List<'a -> 'b>,
    valueList: List<'a>,
    [<OptionalArgument>]_mthd: Applicative
  ): List<'b> = 
    List.collect (fun f -> List.map f valueList) fnList
        

  static member inline InvokeApply (f: '``Applicative<'T->'U>``) (x: '``Applicative<'T>``) : '``Applicative<'U>`` =
    let inline call (
      mthd: ^M,
      fn: ^F,
      input: ^I,
      _outut: ^R
    ) =
      ((^M or ^I or ^F or ^R) : (static member Apply: _ * _ * _ -> _) fn, input, mthd)
    call (Unchecked.defaultof<Applicative>, f, x, Unchecked.defaultof<'``Applicative<'U>``>)
