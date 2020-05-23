namespace NetAtom

open NetOptics
open System
open System.Reactive.Linq

module Stream =
  let empty<'T> = Observable.Empty<'T>()

type [<Sealed>] Stream =
  static member delay (t: TimeSpan) = fun (xO: #IObs<_>) -> xO.Delay t

  static member filter (pred: _ -> _) = fun (xO: #IObs<_>) ->
    xO.Where pred

  static member ifElse (onT: #IObs<_>) = fun (onF: #IObs<_>) (cond: IObs<_>) ->
    cond.Select(fun c -> if c then onT :> IObs<_> else onF :> IObs<_>).Switch()

  static member latestWhen (ticks: IObs<_>) = fun (xO: #IObs<_>) ->
    Observable.WithLatestFrom(ticks, xO, fun _ y -> y)

  static member map2 (xyz: _ -> _ -> _) = fun (xO: #IObs<_>) (yO: #IObs<_>) ->
    Observable.CombineLatest(xO, yO, xyz)
  static member map2 (xyzO: IObs<_ -> _ -> _>) = fun (xO: #IObs<_>) (yO: #IObs<_>) ->
    Observable.CombineLatest(xyzO, xO, yO, fun xyz x y -> xyz x y)

  static member map (xy: _ -> _) = fun (xO: #IObs<_>) ->
    xO.Select xy
  static member map (xy: IObs<_ -> _>) = fun (xO: #IObs<_>) ->
    Stream.map2 id xy xO

  static member merge (xOs: IROL<IObs<_>>) =
    Observable.Merge xOs

  static member startWith (x: 'T) = fun (xO: #IObs<'T>) ->
    xO.StartWith x

  static member subscribe (action: 'T -> unit) = fun (xO: IObs<'T>) ->
    xO.Subscribe action

  static member switchMap (xyO: _ -> IObs<_>) = fun (xO: #IObs<_>) ->
    xO.Select(xyO).Switch()

  static member toProp (xO: IObs<_>) =
    xO.DistinctUntilChanged().Replay(1).RefCount()

type [<Sealed>] Prop =
  static member value x = Observable.Return x

  static member ifElse (onT: #IObs<_>) = fun (onF: #IObs<_>) (cond: IObs<_>) ->
    Stream.ifElse onT onF cond |> Stream.toProp

  static member map (xy: _ -> _) = fun (xO: #IObs<_>) ->
    xO |> Stream.map xy |> Stream.toProp
  static member map (xyO: IObs<_ -> _>) = fun (xO: #IObs<_>) ->
    Stream.map2 id xyO xO |> Stream.toProp

  static member map2 (xyz: _ -> _ -> _) = fun (xO: #IObs<_>) (yO: #IObs<_>) ->
    Stream.map2 xyz xO yO |> Stream.toProp
  static member map2 (xyzO: IObs<_ -> _ -> _>) =
    fun (xO: #IObs<_>) (yO: #IObs<_>) -> Stream.map2 xyzO xO yO |> Stream.toProp

  static member view xyO = fun (xO: #IObs<_>) ->
    Prop.map (Optic.view xyO) xO
  static member view xyO = fun (xO: #IObs<_>) ->
    Prop.map2 Optic.view xyO xO
