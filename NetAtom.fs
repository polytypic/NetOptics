namespace NetAtom

open System
open NetOptics

/// Alias for `IObservable`.
type IObs<'T> = IObservable<'T>

/// An observable view of state.
type IAtom<'S> =
  inherit IObs<'S>
  abstract Modify: Optic<'S, 'F, 'G, 'S> * ('F -> 'G) -> unit
