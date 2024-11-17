namespace Fable.Effect.Data

type HKT = interface end

//[Struct] no F# 4.1 in ffsnip!
type App<'F, 't when 'F :> HKT> = private App of payload: obj

type App<'F, 't1, 't2 when 'F :> HKT> = App<'F, TCons<'t1, 't2>>
and App<'F, 't1, 't2, 't3 when 'F :> HKT> = App<'F, TCons<'t1, 't2, 't3>>
and App<'F, 't1, 't2, 't3, 't4 when 'F :> HKT> = App<'F, TCons<'t1, 't2, 't3, 't4>>

and TCons<'T1, 'T2> = class end
and TCons<'T1, 'T2, 'T3> = TCons<TCons<'T1, 'T2>, 'T3>
and TCons<'T1, 'T2, 'T3, 'T4> = TCons<TCons<'T1, 'T2, 'T3>, 'T4>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module HKT =
  let inline pack (value: 'Fa) : App<'F, 'a> when 'F: (static member Assign: App<'F, 'a> * 'Fa -> unit) = App value

  let inline unpack (App value: App<'F, 'a>) : 'Fa when 'F: (static member Assign: App<'F, 'a> * 'Fa -> unit) =
    value :?> _

  // active pattern variant useful for method definitions
  let inline (|Unpack|) app = unpack app
