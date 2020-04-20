namespace NetOptics

open System.Collections.Generic

/// The internal implementation details of optics are hidden.
type Pipe<'a>

/// Optics are functions composable with the standard `<<` operator.
type Optic<'s, 'a> = Pipe<'a> -> Pipe<'s>

[<AutoOpen>]
module Optic =
  /// Determines whether the optic has a focus on the data.
  val canView: Optic<'s, 'a> -> ('s -> bool)

  /// Extracts the first focus of the optic on the data. Raises when no focus.
  val view: Optic<'s, 'a> -> ('s -> 'a)

  /// Attempts to extract the first focus of the optic on the data.
  val tryView: Optic<'s, 'a> -> ('s -> option<'a>)

  /// Folds over the focuses of the optic on the data.
  val fold: Optic<'s, 'a> -> ('r -> 'a -> 'r) -> 'r -> 's -> 'r

  /// Iterates over the focuses of the optic on the data.
  val iter: Optic<'s, 'a> -> ('a -> unit) -> ('s -> unit)

  /// Maps over the focuses of the optic on the data.
  val over: Optic<'s, 'a> -> ('a -> 'a) -> ('s -> 's)

  /// Sets focuses of the optic on the data.
  val set: Optic<'s, 'a> -> 'a -> ('s -> 's)

  /// Determines whether the optic can be used to remove focuses.
  val canRemove: Optic<'s, 'a> -> ('s -> bool)

  /// Removes focuses of the optic from the data. Raises when cannot remove.
  val remove: Optic<'s, 'a> -> ('s -> 's)

  /// Attempts to remove focuses of the optic from the data.
  val tryRemove: Optic<'s, 'a> -> ('s -> option<'s>)

  /// Extracts an array of all the focuses of the optic on the data.
  val collect: Optic<'s, 'a> -> 's -> IReadOnlyList<'a>

  /// Injects elements from the array to the focuses of the optic on the data.
  val disperse: Optic<'s, 'x> -> IReadOnlyList<'x> -> 's -> 's

  /// Converts an optic to a lens focusing on an array of the focuses.
  val partsOf: Optic<'s, 'a> -> Optic<'s, IReadOnlyList<'a>>

  /// Defines a new lens from a getter and setter.
  val lens: ('s -> 'a) -> ('a -> 's -> 's) -> Optic<'s, 'a>

  /// Defines a new isomorphism from a pair of conversion functions.
  val iso: ('s -> 't) -> ('t -> 's) -> Optic<'s, 't>

  /// Views through isomorphism in inverse direction. Raises on non-isos.
  val review: Optic<'s, 't> -> 't -> 's

  /// Inverts the given isomorphism.
  val invertI: Optic<'s, 't> -> Optic<'t, 's>

  /// A prism with a focus only when it passes the given predicate.
  val whereP: ('a -> bool) -> Optic<'a, 'a>

  /// A non-isomorphism computed from the focus.
  val chooseL: ('s -> Optic<'s, 'a>) -> Optic<'s, 'a>

  /// A choice between two non-isomorphisms depending on the focus.
  val ifElseL: ('s -> bool) -> Optic<'s, 'a> -> Optic<'s, 'a> -> Optic<'s, 'a>

  /// A prism that never has a focus.
  val zeroP: Optic<'s, 's>

  /// The identity isomorphism.
  val idI: Optic<'s, 's>

  /// A lens focusing on the first element of a pair.
  val fstL: Optic<'x * 'y, 'x>

  /// A lens focusing on the second element of a pair.
  val sndL: Optic<'x * 'y, 'y>

  /// A traversal over the elements of a list.
  val elemsT: Optic<IReadOnlyList<'a>, 'a>

  /// A prism focusing on element at given index of a list.
  val atP: int -> Optic<IReadOnlyList<'a>, 'a>

  /// An isomorphism whose focus is reverse of the list.
  val revI: Optic<IReadOnlyList<'a>, IReadOnlyList<'a>>

  /// An isomorphism between string with separators and the separated strings.
  val splitI: char -> Optic<string, IReadOnlyList<string>>
