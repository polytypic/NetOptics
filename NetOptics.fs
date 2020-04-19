namespace NetOptics

open System.Collections.Generic
open System.Linq

type [<Struct>] Context =
  val mutable Over: bool
  val mutable Hit: bool
  val mutable View: obj

type D<'a> = delegate of byref<Context> * 'a -> 'a

type Pipe<'a> = P of D<'a> * inverted: bool

type Optic<'s,'a> = Pipe<'a> -> Pipe<'s>

[<AutoOpen>]
module Helpers =
  let inline nil<'x> = Unchecked.defaultof<'x>
  let inline I inverted p = P (p, inverted)
  let inline O p = I false p

[<AutoOpen>]
module Optic =
  open Helpers

  let inline private viewWith (o: Optic<_, _>) finish =
    let (P (p, _)) = O<|D(fun c x -> c.View <- x; c.Hit <- true; nil<_>)|>o
    fun s ->
      let mutable c = Context ()
      p.Invoke (&c, s) |> ignore
      finish c.Hit c.View

  let canView o = viewWith o <| fun h _ -> h
  let view (o: Optic<'s, 'a>): 's -> 'a = viewWith o <| fun h r ->
    if h then unbox<'a> r else failwith "view"
  let tryView (o: Optic<'s, 'a>): 's -> option<'a>  = viewWith o <| fun h r ->
    if h then unbox<'a> r |> Some else None

  let review (o: Optic<'s, 't>) (y: 't) =
    let (P (p, inverted)) = I true <|D(fun c _ -> y)|>o
    if not inverted then failwith "review"
    let mutable c = Context ()
    c.Over <- true
    p.Invoke (&c, nil<_>)

  let fold (o: Optic<'s, 'a>) (rxr: 'r -> 'a -> 'r) (x: 'r) s =
    let mutable c = Context ()
    let mutable r = x
    let (P (p, _)) = O<|D(fun c x -> r <- rxr r x; nil<_>) |>  o
    p.Invoke (&c, s) |> ignore
    r

  let iter (o: Optic<'s, 'a>) ef =
    let (P (p, _)) = O<|D(fun _ x -> ef x; nil<_>)|>o
    fun s ->
      let mutable c = Context ()
      p.Invoke (&c, s) |> ignore

  let inline private over' (o: Optic<'s, 'a>) fn =
    let (P (p, _)) = O<|D(fun c x -> fn x)|>o
    fun s ->
      let mutable c = Context ()
      c.Over <- true
      p.Invoke (&c, s)

  let over (o: Optic<'s, 'a>) fn = over' o fn
  let set (o: Optic<'s, 'a>) x = over' o <| fun _ -> x

  let inline private removeWith (o: Optic<'s, 'a>) finish =
    let (P (p, _)) = O<|D(fun c _ -> c.Hit <- true; nil<_>)|>o
    fun s ->
      let mutable c = Context ()
      c.Over <- true
      let result = p.Invoke (&c, s)
      finish c.Hit result

  let canRemove (o: Optic<'s, 'a>) = removeWith o <| fun h _ -> not h
  let remove (o: Optic<'s, 'a>) =
    removeWith o <| fun h r -> if h then failwith "remove" else r
  let tryRemove (o: Optic<'s, 'a>) =
    removeWith o <| fun h r -> if h then None else Some r

  let lens get set (P (p, _)) = O<|D(fun c s ->
    let a = p.Invoke (&c, get s)
    if c.Over then set a s else nil<_>)

  let collect (o: Optic<'s, 'a>) s =
    let xs = ResizeArray<_>()
    iter o xs.Add s
    xs.ToArray ()
  let disperse (o: Optic<'s, 'a>) (xs: array<_>) s =
    let mutable i = 0
    over o
     <| fun x -> if i < xs.Length then let x = xs.[i] in i <- i + 1; x else x
     <| s
  let partsOf o = lens (collect o) (disperse o)

  let iso fwd bwd (P (p, inverted)) = I inverted <|D(fun c s ->
    let a = p.Invoke (&c, if inverted then nil<_> else fwd s)
    if c.Over then bwd a else nil<_>)

  let invertI o = iso (review o) (view o)

  let whereP predicate (P (p, _)) =
    O<|D(fun c x -> if predicate x then p.Invoke (&c, x) else x)

  let chooseL (toOptic: 's -> Optic<'s, 'a>) p =
    O<|D(fun c s -> let (P (p, _)) = toOptic s p in p.Invoke (&c, s))

  let ifElseL pred onT onF (p: Pipe<_>) =
    let (P (pT, _)) = onT p
    let (P (pF, _)) = onF p
    O<|D(fun c s -> if pred s then pT.Invoke (&c, s) else pF.Invoke (&c, s))

  let zeroP (_: Pipe<'s>) = O<|D(fun _ (s: 's) -> s)

  let idI p = iso id id p

  let fstL<'x, 'y> p = lens fst (fun x (_: 'x, y: 'y) -> (x, y)) p
  let sndL<'x, 'y> p = lens snd (fun y (x: 'x, _: 'y) -> (x, y)) p

module Collections =
  let elemsOf (ctor: IReadOnlyList<'x> -> 'xs when 'xs :> IEnumerable<'x>)
              (P (p, _): Pipe<'x>) = O<|D(fun c (xs: 'xs) ->
    if c.Over then
      let ys = ResizeArray<_>()
      use e = (xs :> IEnumerable<_>).GetEnumerator ()
      while (e :> IEnumerator<_>).MoveNext () do
        let y = p.Invoke (&c, (e :> IEnumerator<_>).Current)
        if c.Hit then c.Hit <- false else ys.Add y
      ctor ys
    else
      use e = (xs :> IEnumerable<_>).GetEnumerator ()
      while not c.Hit && (e :> IEnumerator<_>).MoveNext () do
        p.Invoke (&c, (e :> IEnumerator<_>).Current) |> ignore
      nil<_>)

  let atOf (ctor: IReadOnlyList<'x> -> 'xs :> IReadOnlyList<'x>) ix
           (P (p, _): Pipe<'x>) = O<|D(fun c (xs: 'xs) ->
    let n = (xs :> IReadOnlyList<'x>).Count
    if c.Over then
      let ys = ResizeArray<_>(n)
      let mutable i = 0
      while i < n do
        let x = xs.[i]
        let y = if i = ix then p.Invoke (&c, x) else x
        if c.Hit then c.Hit <- false else ys.Add y
        i <- i + 1
      ctor ys
    else
      if 0 <= ix && ix < n then p.Invoke (&c, xs.[ix]) |> ignore
      nil<_>)

module Array =
  let ofList (xs: IReadOnlyList<'x>) = xs.ToArray ()
  let elemsT p = Collections.elemsOf ofList p
  let atP ix = Collections.atOf ofList ix
  let revI p = iso Array.rev Array.rev p

module String =
  let splitI (sep: char) =
    let seps = [|sep|]
    iso (fun (s: string) -> s.Split seps) (String.concat <| string sep)
