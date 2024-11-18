namespace Himanoa.Fable.Effect.Data

type State<'s, 'r> =
  | Get of ('s -> 'r)
  | Put of 's * 'r

type StateF =
  interface HKT
  static member Assign(_: App<StateF, 'a>, _: State<'s, 'a>) : unit = ()

module StateAction =
  let wrap (fa: State<'s, 'a>) : App<StateF, 'a> = HKT.pack fa
  let unwrap (app: App<StateF, 'a>) = HKT.unpack app

module State =
  let get<'s> () : Free<StateF, 's, 's> =
    (Get id) |> StateAction.wrap |> Free.liftF

  let put<'s> (s: 's) : Free<StateF, 's, unit> =
    (Put(s, s)) |> StateAction.wrap |> Free.liftF

  type private StateFunc<'s, 'a> = 's -> 's * 'a

  let runState<'s, 'a> (initialState: 's) (program: Free<StateF, _, 'a>) : 's * 'a =
    let interpreter (action: App<StateF, 's>) (k: 's -> StateFunc<'s, 'a>) : StateFunc<'s, 'a> =
      fun state ->
        match StateAction.unwrap action with
        | Get f ->
          let result = f state
          (k result) state
        | Put(newState, _) -> k newState newState

    let pure' (x: 'a) : StateFunc<'s, 'a> = fun s -> (s, x)

    (Free.fold interpreter pure' program) initialState
