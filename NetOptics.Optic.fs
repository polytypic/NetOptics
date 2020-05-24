module NetOptics.Optic

open System
open System.Collections.Generic
open System.Net

type [<Struct>] Context =
  val mutable Hit: bool
  val mutable Over: bool
  val mutable Index: int
  val mutable View: obj

type D<'S, 'T> = delegate of byref<Context> * 'S -> 'T

type Pipe<'S, 'T> = P of D<'S, 'T> * inverted: bool

type t<'S, 'F, 'G, 'T> = Pipe<'F, 'G> -> Pipe<'S, 'T>

type IROL<'T> = IReadOnlyList<'T>

let inline private inc (i: byref<_>) = i <- i + 1

let private toArray (xs: IROL<_>) =
  let n = xs.Count
  let ys = Array.zeroCreate n
  let mutable i = 0
  use e = xs.GetEnumerator ()
  while e.MoveNext () do
    ys.[i] <- e.Current
    inc &i
  ys

let inline private asArray (xs: IROL<_>) =
  match xs with
  | :? array<_> as xs -> xs
  | _ -> toArray xs

let private removeAt i (xs: IROL<_>) =
  let xs = asArray xs
  let n = xs.Length
  let ys = Array.zeroCreate (n-1)
  for j=0 to i-1 do ys.[j] <- xs.[j]
  for j=i+1 to n-1 do ys.[j-1] <- xs.[j]
  ys :> IROL<_>

let private setAt i y (xs: IROL<_>) =
  let ys = toArray xs
  ys.[i] <- y
  ys :> IROL<_>

let inline private asList (xs: _[]) = xs :> IROL<_>

let inline private append (l, r) =
  if Array.length l = 0 then r
  elif Array.length r = 0 then l
  else Array.append l r

let inline private constant x _ = x

let inline private nil<'x> = Unchecked.defaultof<'x>

let inline private I inverted p = P (p, inverted)
let inline private O p = I false p

let inline private viewWith (o: t<_, _, _, _>) finish =
  let (P (p, _)) = O<|D(fun c x -> c.View <- x; c.Hit <- true; nil<_>)|>o
  fun s ->
    let mutable c = Context ()
    p.Invoke (&c, s) |> ignore
    finish c.Hit c.View

let canView o = viewWith o <| fun h _ -> h
let view (o: t<_, 'F, _, _>): _ -> _ = viewWith o <| fun h r ->
  if h then unbox<'F> r else failwith "view"
let tryView (o: t<_, 'F, _, _>): _ -> option<_> = viewWith o <| fun h r ->
  if h then unbox<'F> r |> Some else None

let review (anIso: t<_, _, 'G, _>) =
  let (P (p, inverted)) = I true <|D(fun c _ -> unbox<'G> c.View)|>anIso
  if not inverted then failwith "review"
  fun (f: 'G) ->
    let mutable c = Context (Over = true, View = f)
    p.Invoke (&c, nil<_>)

let fold zero plus (o: t<_, _, _, _>) s =
  let mutable c = Context ()
  let mutable r = zero
  let (P (p, _)) = O<|D(fun _ x -> r <- plus r x; nil<_>) |>  o
  p.Invoke (&c, s) |> ignore
  r

let count o = fold 0 (fun n _ -> n + 1) o

let iter (o: t<_, _, _, _>) action =
  let (P (p, _)) = O<|D(fun _ x -> action x; nil<_>)|>o
  fun s ->
    let mutable c = Context ()
    p.Invoke (&c, s) |> ignore

let inline private overWith (o: t<_, _, _, _>) mapping finish =
  let (P (p, _)) = O<|D(fun _ x -> mapping x)|>o
  fun s ->
    let mutable c = Context (Over = true)
    let r = p.Invoke (&c, s)
    finish c.Hit r s

let canOver o fn = overWith o fn <| fun h _ _ -> not h
let over o fn = overWith o fn <| fun h r _ -> if h then failwith "over" else r
let overDefault o fn = overWith o fn <| fun h r s -> if h then s else r
let tryOver o fn = overWith o fn <| fun h r _ -> if h then None else Some r

let canSet o value = canOver o (constant value)
let set o value = over o (constant value)
let setDefault o value = overDefault o (constant value)
let trySet o alue = tryOver o (constant alue)

let removeP: t<_, _, _, _> = fun _ -> O<|D(fun c _ -> c.Hit <- c.Over; nil<_>)

let removeIfL predicate (P (p, _)) =
  O<|D(fun c x ->
    let y = p.Invoke (&c, x)
    if c.Over
    then if predicate y then c.Hit <- true; nil<_> else y
    else nil<_>)
let removeEqL value = removeIfL ((=) value)

let canRemove (o: t<_, _, _, _>) = canOver (o << removeP) id
let remove (o: t<_, _, _, _>) = over (o << removeP) id
let tryRemove (o: t<_, _, _, _>) = tryOver (o << removeP) id
let removeDefault (o: t<_, _, _, _>) = overDefault (o << removeP) id

let lens project inject (P (p, _)) = O<|D(fun c s ->
  let b = p.Invoke (&c, project s)
  if c.Over then inject b s else nil<_>)

let foldLens aFold aTraversal = lens (aFold aTraversal) (set aTraversal)

let collect o s =
  let xs = ResizeArray<_>()
  iter o xs.Add s
  xs.ToArray() :> IROL<_>

let disperseKeep (o: t<_, _, _, _>) (values: #IROL<_>) =
  let (P (p, _)) = O<|D(fun c x ->
    let i = c.Index
    if i < values.Count
    then let x = values.[i] in inc &c.Index; x else x)|>o
  fun s ->
    let mutable c = Context (Over = true)
    let r = p.Invoke (&c, s)
    if c.Hit then failwith "disperseKeep"
    r
let disperse (o: t<_, _, _, _>) (values: #IROL<_>) =
  let (P (p, _)) = O<|D(fun c _ ->
    let i = c.Index
    if i < values.Count
    then let x = values.[i] in inc &c.Index; x
    else c.Hit <- true; nil<_>)|>o
  fun s ->
    let mutable c = Context (Over = true)
    let r = p.Invoke (&c, s)
    if c.Hit then failwith "disperse"
    r

let partsOf: t<_, _, _, _> -> t<_, IROL<_>, #IROL<_>, _> =
  fun o -> lens (collect o) (disperse o)
let partsOfKeep: t<_, _, _, _> -> t<_, IROL<_>, #IROL<_>, _> =
  fun o -> lens (collect o) (disperseKeep o)

let iso forward backward (P (p, inverted)) =
  I inverted <|D(fun c s ->
  let b = p.Invoke (&c, if inverted then nil<_> else forward s)
  if c.Over then backward b else nil<_>)

let rereadI fn = iso fn id
let rewriteI fn = iso id fn
let normalizeI fn = iso fn fn

let prism (gt: 'G -> 'T) (stf: 'S -> Choice<'T, 'F>) (P (p, _)) =
  O<|D(fun c s ->
    match stf s with
    | Choice1Of2 t -> t
    | Choice2Of2 f ->
      let g = p.Invoke (&c, f)
      if c.Over then gt g else nil<_>)

let invertI anIso = iso (review anIso) (view anIso)

let whereP predicate (P (p, _)) =
  O<|D(fun c x -> if predicate x then p.Invoke (&c, x) else x)

let exists predicate o = canView (o << whereP predicate)
let forall predicate o = exists (predicate >> not) o >> not

let choose (toOptic: _ -> t<_, _, _, _>) p =
  O<|D(fun c s -> let (P (p, _)) = toOptic s p in p.Invoke (&c, s))

let ifElse predicate onTrue onFalse (p: Pipe<_, _>) =
  let (P (pT, _)) = onTrue p
  let (P (pF, _)) = onFalse p
  O<|D(fun c s -> if predicate s then pT.Invoke (&c, s) else pF.Invoke (&c, s))

let zeroP: t<_, _, _, _> = fun _ -> O<|D(fun _ s -> s)

let idI: t<_, _, _, _> = fun p -> iso id id p

let beforeL action (P (p, _)) = O<|D(fun c s ->
  if not c.Over then action s
  p.Invoke (&c, s))

let fstL: t<_, _, _, _> = fun p -> lens fst (fun x (_, y) -> (x, y)) p
let sndL: t<_, _, _, _> = fun p -> lens snd (fun y (x, _) -> (x, y)) p

let inline private sub (xs: _[]) n =
  if n < xs.Length then Array.sub xs 0 n :> IROL<_> else xs :> IROL<_>

let subT offset count (t: t<_, _, _, _>) =
  let offset = max 0 offset
  let count = max 0 count
  let limit = uint32 offset + uint32 count
  fun (P (p, _)) -> O<|D(fun c s ->
    let mutable i = -1
    let (P (p, _)) = O<|D(fun c x ->
      inc &i
      if offset <= i && uint32 i < limit then p.Invoke (&c, x) else x)|>t
    p.Invoke (&c, s))

let takeT count = subT 0 count
let dropT count = subT count Int32.MaxValue

let indexedT (t: t<_, _, _, _>) = fun (P (p, _)) -> O<|D(fun c s ->
  let mutable i = -1
  let (P (p, _)) = O<|D(fun c x ->
    inc &i
    p.Invoke (&c, struct (i, x)))|>t
  p.Invoke (&c, s))

let elemsT: t<#IROL<_>, _, _, _> = fun (P (p, _)) -> O<|D(fun c xs ->
  let n = xs.Count
  if c.Over then
    let xs = asArray xs
    let ys = Array.zeroCreate n
    let mutable i = 0
    let mutable j = 0
    while i < n do
      let y = p.Invoke (&c, xs.[i])
      if c.Hit then c.Hit <- false else ys.[j] <- y; inc &j
      inc &i
    sub ys j
  else
    let mutable i = 0
    while not c.Hit && i < n do
      p.Invoke (&c, xs.[i]) |> ignore
      inc &i
    nil<_>)

let inline at ix (p: D<_, _>) (c: byref<Context>) (xs: #IROL<_>) =
  let n = xs.Count
  if c.Over then
    if 0 <= ix && ix < n
    then let y = p.Invoke (&c, xs.[ix])
         if c.Hit
         then c.Hit <- false; removeAt ix xs
         else setAt ix y xs
    else xs :> IROL<_>
  else
    if 0 <= ix && ix < n then p.Invoke (&c, xs.[ix]) |> ignore
    nil<_>

let atP ix: t<#IROL<_>, _, _, _> =
  fun (P (p, _)) -> O<|D(fun c xs -> at ix p &c xs)
let atRefP ix: t<#IROL<_>, _, _, _> =
  fun (P (p, _)) -> O<|D(fun c xs -> at !ix p &c xs)

let optionP p = ifElse Option.isNone removeP (rereadI Option.get << idI) p

let removeAsNoneL: t<_, _, _, _> = fun (P (p, _)) ->
  O<|D(fun c x ->
    let y = p.Invoke (&c, x)
    if c.Over
    then if c.Hit then c.Hit <- false; None else Some y
    else nil<_>)

let noneAsRemoveL: t<_, _, _, _> = fun (P (p, _)) ->
  O<|D(fun c x ->
    let yO = p.Invoke (&c, x)
    if c.Over
    then match yO with
         | None -> c.Hit <- true; nil<_>
         | Some y -> y
    else nil<_>)

let someP p = removeAsNoneL << optionP <| p

let choice1of2P: t<_, _, _, _> = fun p ->
  prism Choice1Of2
   <| function Choice1Of2 x -> Choice2Of2 x
             | Choice2Of2 x -> Choice1Of2 (Choice2Of2 x)
   <| p
let choice2of2P: t<_, _, _, _> = fun p ->
  prism Choice2Of2
   <| function Choice1Of2 x -> Choice1Of2 (Choice1Of2 x)
             | Choice2Of2 x -> Choice2Of2 x
   <| p

let oneAsSomeI p =
  iso <| fun (xs: #IROL<_>) -> if xs.Count = 1 then Some xs.[0] else None
      <| function None -> [||] :> IROL<_> | Some x -> [|x|] :> IROL<_>
      <| p

let isOrI falsy truthy =
  iso <| (=) truthy <| function false -> falsy | true -> truthy

let toDefault value = Option.defaultValue value
let ofDefault value v = if v = value then None else Some v

let toDefaultI value = rereadI (toDefault value)
let ofDefaultI value = rewriteI (ofDefault value)
let defaultI value = iso (toDefault value) (ofDefault value)

let arrayI: t<#IROL<_>, _[], _[], _> = fun p -> iso asArray asList p
let rolistI: t<_[], _, #IROL<_>, _[]> = fun p -> iso asList asArray p

let inline private pair f1 f2 (v1, v2) = (f1 v1, f2 v2)
let pairI i1 i2 = iso (pair (view i1) (view i2)) (pair (review i1) (review i2))

let inline private rev xs = xs |> asArray |> Array.rev |> asList
let revI: t<#IROL<_>, _, #IROL<_>, _> = fun p -> iso rev rev p

let splitI (sep: string) =
  let seps = [|sep|]
  iso (fun (s: string) -> s.Split(seps, StringSplitOptions.None) |> asList)
      (String.concat sep: #IROL<_> -> _)

let partitionI predicate: t<#IROL<_>, _, #IROL<_> * #IROL<_>, _> =
  arrayI << iso (Array.partition predicate) append << pairI rolistI rolistI
let filterL predicate: t<_, _, _, _> = partitionI predicate << fstL
let rejectL predicate: t<_, _, _, _> = partitionI predicate << sndL

let elemsI i: t<#IROL<_>, _, #IROL<_>, _> =
  arrayI << iso (Array.map (view i)) (Array.map (review i)) << rolistI

let inline private splitAtWith fn xs =
  let n = Array.length xs
  let i = fn n
  if i = 0 then ([||], xs) elif i = n then (xs, [||]) else Array.splitAt i xs
let inline private splitAt i =
  splitAtWith <| if i < 0 then max 0 << ((+) (i + 1)) else min i
let splitAtI i: t<#IROL<_>, _, #IROL<_> * #IROL<_>, _> =
  arrayI << iso (splitAt i) append << pairI rolistI rolistI
let prependL: t<#IROL<_>, _, #IROL<_>, _> = fun p -> splitAtI 0 << fstL <| p
let appendL: t<#IROL<_>, _, #IROL<_>, _> = fun p -> splitAtI -1 << sndL <| p

let findL predicate =
  choose <| fun xs ->
    match Seq.tryFindIndex predicate xs with
    | Some i -> atP i << rereadI Some
    | None -> appendL << oneAsSomeI << removeAsNoneL

let findP predicate = findL predicate << optionP

let containsL value =
  findL ((=) value) << noneAsRemoveL << isOrI None (Some value)

let andAlso (second: t<_, _, _, _>) (first: t<_, _, _, _>) (p: Pipe<_, _>) =
  let (P (p1, _)) = first p
  let (P (p2, _)) = second p
  O<|D(fun c s ->
    let s = p1.Invoke (&c, s)
    if c.Over then
      if c.Hit then nil<_> else p2.Invoke (&c, s)
    else
      if not c.Hit then p2.Invoke (&c, s) |> ignore
      nil<_>)

let orElse (o2: t<_, _, _, _>) (o1: t<_, _, _, _>) = ifElse (canView o1) o1 o2

let pairL o1 o2 =
  lens <| fun s -> (view o1 s, view o2 s)
       <| fun (v1, v2) s -> set o2 v2 (set o1 v1 s)

let indexedI: t<#IROL<_>, _, #IROL<int * _>, _> = fun p ->
  arrayI
   << iso (Array.mapi <| fun i x -> (i, x))
          (Array.distinctBy fst >> Array.sortBy fst >> Array.map snd) // TODO: opt
   << rolistI
   <| p

let truncateI = iso float int<float>

let optionI total =
  iso <| Option.map (view total)
      <| Option.map (review total)

let private dropPrefix (prefix: string) (string: string) =
  if string.StartsWith prefix
  then Some (string.Substring prefix.Length)
  else None

let dropPrefixI (prefix: string) = iso <| dropPrefix prefix <| (+) prefix

let private replace (inn: string) (out: string) (s: string) = s.Replace (inn, out)
let replaceI inn out = iso (replace inn out) (replace out inn)

let subsetI predicate =
  rereadI (fun inn -> if predicate inn then Some inn else None)

let uncoupleI (sep: string) =
  iso <| fun (s: string) ->
           match s.IndexOf sep with
           | -1 -> (s, "")
           | i -> (s.Substring (0, i), s.Substring (i + sep.Length))
      <| fun (l: string, r: string) ->
           if r = "" then l else (l + sep + r)

let urlDecodeI = iso WebUtility.UrlDecode WebUtility.UrlEncode
let urlEncodeI = iso WebUtility.UrlEncode WebUtility.UrlDecode

let private ofMultiMap m =
  Map.toArray m
   |> Array.map (fun (k, vs) -> vs |> asArray |> Array.map (fun v -> (k, v)))
   |> Array.concat
   |> asList
let private toMultiMap kvs =
  asArray kvs
   |> Array.groupBy fst
   |> Array.map (fun (k, kvs) -> (k, Array.map snd kvs |> asList))
   |> Map.ofArray

let toMultiMapI: t<#IROL<_>, _, _, _> = fun p -> iso toMultiMap ofMultiMap p
let ofMultiMapI: t<_, _, #IROL<_>, _> = fun p -> iso ofMultiMap toMultiMap p

let orI secondary primary =
  iso <| fun x ->
           match view primary x with
           | Some y -> y
           | None -> view secondary x
      <| review primary

let querystringI =
  dropPrefixI "?" |> orI idI
   << replaceI "+" "%20"
   << splitI "&"
   << elemsI (uncoupleI "=" << pairI urlDecodeI urlDecodeI)
   << toMultiMapI
