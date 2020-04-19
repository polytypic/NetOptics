namespace NetOptics

type Pipe<'a>

type Optic<'s,'a> = Pipe<'a> -> Pipe<'s>

[<AutoOpen>]
module Optic =
  val canView: Optic<'s, 'a> -> ('s -> bool)
  val    view: Optic<'s, 'a> -> ('s -> 'a) // raises unless canView
  val tryView: Optic<'s, 'a> -> ('s -> option<'a>)

  val fold: Optic<'s, 'a> -> ('r -> 'a -> 'r) -> 'r -> 's -> 'r
  val iter: Optic<'s, 'a> -> ('a -> unit) -> ('s -> unit)

  val over: Optic<'s, 'a> -> ('a -> 'a) -> ('s -> 's)
  val set:  Optic<'s, 'a> ->        'a  -> ('s -> 's)

  val canRemove: Optic<'s, 'a> -> ('s -> bool)
  val    remove: Optic<'s, 'a> -> ('s -> 's) // raises unless canRemove
  val tryRemove: Optic<'s, 'a> -> ('s -> option<'s>)

  val collect: Optic<'s, 'a> -> 's -> 'a[]
  val disperse: Optic<'s, 'a> -> 'a[] -> 's -> 's
  val partsOf: Optic<'s, 'a> -> Optic<'s, 'a[]>

  val lens: ('s -> 'a) -> ('a -> 's -> 's) -> Optic<'s, 'a>

  val iso: ('s -> 't) -> ('t -> 's) -> Optic<'s, 't>
  val review: Optic<'s, 't> -> 't -> 's // raises unless isomorphism

  val invertI: Optic<'s, 't> -> Optic<'t, 's> // requires an isomorphism

  val whereP: ('a -> bool) -> Optic<'a, 'a>

  val chooseL: ('s -> Optic<'s, 'a>) -> Optic<'s, 'a>

  val zeroP: Optic<'s, 's>

  val idI: Optic<'s, 's>

  val fstL: Optic<'x * 'y, 'x>
  val sndL: Optic<'x * 'y, 'y>

module Array =
  val atP: int -> Optic<'a[], 'a>

  val elemsT: Optic<'a[], 'a>

  val revI: Optic<'a[], 'a[]>

module String =
  val splitI: char -> Optic<string, string[]>
