namespace NetOptics

type [<Struct>] Context =
  val mutable Over: bool
  val mutable Invert: bool
  val mutable Hit: bool
  val mutable View: obj

type D<'a> = delegate of byref<Context> * 'a -> 'a

type Pipe<'a> = P of D<'a>

type Optic<'s,'a> = Pipe<'a> -> Pipe<'s>

[<AutoOpen>]
module Helpers =
  let inline nil<'x> = Unchecked.defaultof<'x>

[<AutoOpen>]
module Optic =
  open Helpers

  let inline private viewWith (o: Optic<_, _>) finish =
    let (P p) = P<|D(fun c x -> c.View <- x; c.Hit <- true; nil<_>)|>o
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
    let (P p) = P<|D(fun c _ -> y)|>o
    let mutable c = Context ()
    c.Over <- true
    c.Invert <- true
    p.Invoke (&c, nil<_>)

  let fold (o: Optic<'s, 'a>) (rxr: 'r -> 'a -> 'r) (x: 'r) s =
    let mutable c = Context ()
    let mutable r = x
    let (P p) = P<|D(fun c x -> r <- rxr r x; nil<_>) |>  o
    p.Invoke (&c, s) |> ignore
    r

  let iter (o: Optic<'s, 'a>) ef =
    let (P p) = P<|D(fun _ x -> ef x; nil<_>)|>o
    fun s ->
      let mutable c = Context ()
      p.Invoke (&c, s) |> ignore

  let inline private over' (o: Optic<'s, 'a>) fn =
    let (P p) = P<|D(fun c x -> fn x)|>o
    fun s ->
      let mutable c = Context ()
      c.Over <- true
      p.Invoke (&c, s)

  let over (o: Optic<'s, 'a>) fn = over' o fn
  let set (o: Optic<'s, 'a>) x = over' o <| fun _ -> x

  let inline private removeWith (o: Optic<'s, 'a>) finish =
    let (P p) = P<|D(fun c _ -> c.Hit <- true; nil<_>)|>o
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

  let lens get set (P p) = P<|D(fun c s ->
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

  let iso fwd bwd (P p) = P<|D(fun c s ->
    let a = p.Invoke (&c, if c.Invert then nil<_> else fwd s)
    if c.Over then bwd a else nil<_>)

  let invertI o = iso (review o) (view o)

  let whereP predicate (P p) =
    P<|D(fun c x -> if predicate x then p.Invoke (&c, x) else x)

  let chooseL (toOptic: 's -> Optic<'s, 'a>) p =
    P<|D(fun c s -> let (P p) = toOptic s p in p.Invoke (&c, s))

  let zeroP (_: Pipe<'s>) = P<|D(fun _ (s: 's) -> s)

  let idI p = iso id id p

  let fstL<'x, 'y> p = lens fst (fun x (_: 'x, y: 'y) -> (x, y)) p
  let sndL<'x, 'y> p = lens snd (fun y (x: 'x, _: 'y) -> (x, y)) p

module Array =
  let elemsT (P p) = P<|D(fun c (xs: array<'a>) ->
    let n = xs.Length
    if c.Over then
      let ys = Array.zeroCreate n
      let mutable i = 0
      let mutable j = 0
      while i < n do
        let y = p.Invoke (&c, xs.[i])
        if c.Hit then
          c.Hit <- false
          i <- i + 1
        else
          ys.[j] <- y
          i <- i + 1
          j <- j + 1
      if i <> j then Array.sub ys 0 j else ys
    else
      let mutable i = 0
      while not c.Hit && i < n do
        p.Invoke (&c, xs.[i]) |> ignore
        i <- i + 1
      nil<_>)

  let atP ix (P p) = P<|D(fun c (xs: array<'a>) ->
    let n = xs.Length
    if c.Over then
      let ys = Array.zeroCreate n
      let mutable i = 0
      let mutable j = 0
      while i < n do
        let x = xs.[i]
        let y = if i = ix then p.Invoke (&c, x) else x
        if c.Hit then
          c.Hit <- false
          i <- i + 1
        else
          ys.[j] <- y
          i <- i + 1
          j <- j + 1
      if i <> j then
        c.Hit <- false
        Array.sub ys 0 j
      else
        ys
    else
      if 0 <= ix && ix < n then p.Invoke (&c, xs.[ix]) |> ignore
      nil<_>)

  let revI p = iso Array.rev Array.rev p

module String =
  let splitI (sep: char) =
    let seps = [|sep|]
    iso (fun (s: string) -> s.Split seps) (String.concat <| string sep)
