module NetOptics.History

open System
open System.Collections.Generic

type Config<'S> =
  { MaxCount: int
    Equals: 'S -> 'S -> bool
    ReplacePeriod: TimeSpan }

let defaults<'S> () =
  { MaxCount = Int32.MaxValue
    Equals =
      let eq = EqualityComparer<'S>.Default
      fun l r -> eq.Equals(l, r)
    ReplacePeriod = TimeSpan.Zero }

type PreparedConfig<'S> =
  { Limit: int
    Ignore: 'S -> 'S -> bool
    Grace: TimeSpan }

let prepare config =
  { Limit = (max 1 config.MaxCount) - 1
    Ignore = config.Equals
    Grace = max TimeSpan.Zero config.ReplacePeriod }

type t<'S> =
  { Config: PreparedConfig<'S>
    Index: int
    Stamps: DateTime[]
    Values: 'S[] }

let init (configure: Config<'S> -> Config<'S>) initial =
  { Config = defaults () |> configure |> prepare
    Index = 0
    Stamps = [|DateTime.UtcNow|]
    Values = [|initial|] }

let indexMax h = h.Values.Length - 1

let private setIndex i h = { h with Index = max 0 <| min i (indexMax h) }

let index p = Optic.lens (fun h -> h.Index) setIndex p

let private setPresent v h =
  let i = h.Index
  let c = h.Config
  let vs = h.Values
  if c.Ignore vs.[i] v then h else
    let t = DateTime.UtcNow
    let ts = h.Stamps
    let j = if t - ts.[i] < c.Grace then i else i + 1
    let j0 = max 0 (j - c.Limit)
    let n = j - j0
    { Config = c
      Index = n
      Stamps = Array.append (Array.sub ts j0 n) [|t|]
      Values = Array.append (Array.sub vs j0 n) [|v|] }

let present p = Optic.lens (fun h -> h.Values.[h.Index]) setPresent p

let count h = h.Values.Length

let undoIndex = index

let undoForget h =
  let i = h.Index
  let ts = h.Stamps
  let vs = h.Values
  let n = vs.Length - i
  { Config = h.Config
    Index = 0
    Stamps = Array.sub ts i n
    Values = Array.sub vs i n }

let redoIndex p =
  Optic.lens
   <| fun h -> indexMax h - h.Index
   <| fun i h -> setIndex (indexMax h - i) h
   <| p

let redoForget h =
  let i = h.Index
  let ts = h.Stamps
  let vs = h.Values
  let n = vs.Length - i
  { Config = h.Config
    Index = h.Index
    Stamps = Array.sub ts i n
    Values = Array.sub vs i n }