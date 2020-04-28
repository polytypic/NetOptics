module NetOptics.History

open System

/// History collection type.
type t<'S>

/// Configuration options for updating history.
type Config<'S> =
  { MaxCount: int
    Equals: 'S -> 'S -> bool
    ReplacePeriod: TimeSpan }

/// Creates a history collection with given initial value and configuration.
val init: (Config<'S> -> Config<'S>) -> 'S -> t<'S>

/// A lens that focuses on the present value of history.
val present: Optic.t<t<'S>, 'S, 'S, t<'S>>

/// Removes entries prior to present from history.
val undoForget: t<'S> -> t<'S>

/// A lens that focuses on the undo position of history.
val undoIndex: Optic.t<t<'S>, int, int, t<'S>>

/// Removes entries following present from history.
val redoForget: t<'S> -> t<'S>

/// A lens that focuses on the redo position of history.
val redoIndex: Optic.t<t<'S>, int, int, t<'S>>

/// Number of entries in history.
val count: t<'S> -> int

/// A lens that focuses on the index of present.
val index: Optic.t<t<'S>, int, int, t<'S>>

/// Maximum number for index to history.
val indexMax: t<'S> -> int
