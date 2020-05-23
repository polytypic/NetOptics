namespace NetAtom

open NetOptics
open System

module Stream =
  val empty<'T> : IObs<'T>

type [<Sealed>] Stream =
  static member delay: TimeSpan -> (#IObs<'T> -> IObs<'T>)

  static member filter: ('T -> bool) -> (#IObs<'T> -> IObs<'T>)

  static member ifElse: #IObs<'T> -> (#IObs<'T> -> #IObs<bool> -> IObs<'T>)

  static member latestWhen: IObs<_> -> (#IObs<'T> -> IObs<'T>)

  static member map:     ('S -> 'T) -> (#IObs<'S> -> IObs<'T>)
  static member map: IObs<'S -> 'T> -> (#IObs<'S> -> IObs<'T>)

  static member map2:     ('S -> 'T -> 'U) -> (#IObs<'S> -> #IObs<'T> -> IObs<'U>)
  static member map2: IObs<'S -> 'T -> 'U> -> (#IObs<'S> -> #IObs<'T> -> IObs<'U>)

  static member merge: IROL<IObs<'T>> -> IObs<'T>

  static member startWith: 'T -> (#IObs<'T> -> IObs<'T>)

  static member subscribe: ('T -> unit) -> (IObs<'T> -> IDisposable)

  static member switchMap: ('S -> IObs<'T>) -> (#IObs<'S> -> IObs<'T>)

  static member toProp: IObs<'T> -> IObs<'T>

type [<Sealed>] Prop =
  static member ifElse: #IObs<'T> -> (#IObs<'T> -> #IObs<bool> -> IObs<'T>)

  static member map:     ('S -> 'T) -> (#IObs<'S> -> IObs<'T>)
  static member map: IObs<'S -> 'T> -> (#IObs<'S> -> IObs<'T>)

  static member map2:     ('S -> 'T -> 'U) -> (#IObs<'S> -> #IObs<'T> -> IObs<'U>)
  static member map2: IObs<'S -> 'T -> 'U> -> (#IObs<'S> -> #IObs<'T> -> IObs<'U>)

  static member value: 'T -> IObs<'T>

  static member view:      Optic<'S, 'F, 'G, 'T>  -> (#IObs<'S> -> IObs<'F>)
  static member view: IObs<Optic<'S, 'F, 'G, 'T>> -> (#IObs<'S> -> IObs<'F>)
