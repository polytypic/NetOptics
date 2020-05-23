namespace NetAtom

open System.Collections.Generic
open System.Reactive.Linq
open System.Reactive.Subjects
open NetOptics

type [<Sealed>] Atom =
  static member create (x: 's) =
    let updates = new Subject<'s -> 's>()
    let property =
      updates.Scan(x, fun x fn -> fn x).DistinctUntilChanged().Replay(1)
    property.Connect() |> ignore
    updates.OnNext id
    {new IAtom<'s> with
      member t.Subscribe o = property.Subscribe o
      member t.Modify (optic, fn) = updates.OnNext (Optic.overDefault optic fn)}

  static member view (xyO: Optic<_, _>) = fun (xA: IAtom<_>) ->
    let property =
      xA
       |> Stream.filter (Optic.canView xyO)
       |> Stream.map (Optic.view xyO)
       |> Stream.toProp
    {new IAtom<'x> with
      member t.Subscribe o = property.Subscribe o
      member t.Modify (optic, fn) = xA.Modify (xyO << optic, fn)}

  static member view (xyO: IObs<_>) = fun (xA: IAtom<_>) ->
    let mutable hack = Unchecked.defaultof<_>
    let property =
      xyO.CombineLatest(xA, fun xyO x -> hack <- xyO; Optic.tryView xyO x)
       |> Stream.filter Option.isSome
       |> Stream.map Option.get
       |> Stream.toProp
    {new IAtom<_> with
      member t.Subscribe o = property.Subscribe o
      member t.Modify (optic, fn) = xA.Modify (hack << optic, fn)}

  static member modifyAt o (xA: IAtom<_>) fn = xA.Modify (o, fn)
  static member modify xA fn = Atom.modifyAt Optic.idI xA fn
  static member modifyAct xA fn = fun _ -> Atom.modify xA fn

  static member remove xA = Atom.modifyAt Optic.removeP xA id
  static member removeAct xA = fun _ -> Atom.remove xA

  static member setAt o xA x = Atom.modifyAt o xA <| fun _ -> x
  static member setAtAct o xA x = fun _ -> Atom.setAt o xA x

  static member set xA x = Atom.setAt Optic.idI xA x
  static member setAct xA x = Atom.setAtAct Optic.idI xA x

  static member mapByKey keyOf mapping (xsA: IAtom<IROL<_>>) =
    let entries = new Dictionary<_, _>()
    let mutable prevKs = [||]
    let mutable prevEs = [||]
    let mutable source = xsA
    source <-
      source
      |> Atom.view (Optic.beforeL <| fun (items: IROL<_>) ->
          let n = items.Count
          let nextKs = Array.zeroCreate n
          let nextEs = Array.zeroCreate n
          let mutable changed = prevEs.Length <> n
          for i=0 to n-1 do
            let item = items.[i]
            let key = keyOf item
            nextKs.[i] <- key
            let mutable prevIx = Unchecked.defaultof<_>
            if entries.TryGetValue (key, &prevIx) then
              let j = !prevIx
              nextEs.[i] <- prevEs.[j]
              if i <> j then
                prevIx := i
                changed <- true
            else
              let nextIx = ref i
              let view = Atom.view (Optic.atRefP nextIx) source
              nextEs.[i] <- mapping key view
              entries.Add (key, nextIx)
              changed <- true
          if changed then
            for i=0 to prevEs.Length-1 do
              let key = prevKs.[i]
              let mutable prevIx = Unchecked.defaultof<_>
              if entries.TryGetValue (key, &prevIx) then
                let j = !prevIx
                if i = j && (n <= j || nextKs.[j] <> key) then
                  prevIx := -1
                  entries.Remove key |> ignore
            prevKs <- nextKs
            prevEs <- nextEs)
    source.Select(fun _ -> prevEs :> IROL<_>)
      .DistinctUntilChanged().Replay(1).RefCount()

  static member map mapping vs =
    vs
     |> Atom.view Optic.indexedI
     |> Atom.mapByKey fst (fun _ -> Atom.view Optic.sndL >> mapping)
