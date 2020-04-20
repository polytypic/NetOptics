namespace NetAtom

open NetOptics

type [<Sealed>] Atom =
  /// Creates a new stateful view with given initial value.
  static member create: 'S -> IAtom<'S>

  /// Creates a stateless subview of state through the given lens.
  static member view: Optic<'S, 'F> -> (IAtom<'S> -> IAtom<'F>)

  /// Creates a stateless subview of state through the given observable lens.
  static member view: IObs<Optic<'S, 'F>> -> (IAtom<'S> -> IAtom<'F>)

  /// Requests state update with the given function.
  static member modify: IAtom<'S> -> ('S -> 'S) -> unit

  /// Requests state update with the given function and optic.
  static member modifyAt: Optic<'S, 'F, 'G, 'S>
                       -> IAtom<'S>
                       -> ('F -> 'G)
                       -> unit

  /// Requests state update with the given value.
  static member set: IAtom<'S> -> 'S -> unit

  /// Requests state update with the given value and optic.
  static member setAt: Optic<'S, 'F, 'G, 'S> -> IAtom<'S> -> 'G -> unit

  /// Requests state update to remove viewed substate.
  static member remove: IAtom<'S> -> unit

  /// Maps a view of a list of keyed elements to an observable of values
  /// constructed from views of list elements.  The `keyOf` and `mapping`
  /// functions are considered to be pure and the results of `mapping` are
  /// cached.
  static member mapByKey: keyOf: ('S -> 'K)
                       -> mapping: ('K -> IAtom<'S> -> 'T)
                       -> IAtom<IROL<'S>>
                       -> IObs<IROL<'T>> when 'K: equality

  /// Maps a view of a list of elements to an observable of values constructed
  /// from views of list elements.  The `mapping` function is considered to be
  /// pure.  You should always prefer `mapByKey` when list elements have a
  /// unique keys to get better caching of results.
  static member map: mapping: (IAtom<'S> -> 'T)
                  -> IAtom<IROL<'S>>
                  -> IObs<IROL<'T>>