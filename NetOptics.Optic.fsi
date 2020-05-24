module NetOptics.Optic

open System.Collections.Generic

/// The internal implementation details of optics are hidden.
type Pipe<'S, 'T>

/// Optics are functions composable with the standard `<<` operator.
type t<'S, 'F, 'G, 'T> = Pipe<'F, 'G> -> Pipe<'S, 'T>
/// Optics are functions composable with the standard `<<` operator.
type t<'S, 'F> = t<'S, 'F, 'F, 'S>
/// Optics are functions composable with the standard `<<` operator.
type t<'S> = t<'S, 'S>

/// Short alias for the `IReadOnlyList` type.
type IROL<'T> = IReadOnlyList<'T>

/// Determines whether the optic has a focus on the data.
val canView: t<'S, 'F, 'G, 'T> -> ('S -> bool)
/// Extracts the first focus of the optic on the data. Raises when no focus.
val view: t<'S, 'F, 'G, 'T> -> ('S -> 'F)
/// Attempts to extract the first focus of the optic on the data.
val tryView: t<'S, 'F, 'G, 'T> -> ('S -> option<'F>)

/// Determines whether the focuses of the optic on the data can be updated.
val canOver: t<'S, 'F, 'G, 'T> -> mapping: ('F -> 'G) -> ('S -> bool)
/// Updates the focuses of the optic on the data. Raises when cannot.
val over: t<'S, 'F, 'G, 'T> -> mapping: ('F -> 'G) -> ('S -> 'T)
/// Updates the focuses of the optic on the data. Returns input when cannot.
val overDefault: t<'S, 'F, 'G, 'S> -> mapping: ('F -> 'G) -> ('S -> 'S)
/// Attempts to update the focuses of the optic on the data.
val tryOver: t<'S, 'F, 'G, 'T> -> mapping: ('F -> 'G) -> ('S -> option<'T>)

/// Determines whether the focuses of the optic on the data can be set.
val canSet: t<'S, 'F, 'G, 'T> -> value: 'G -> ('S -> bool)
/// Sets the focuses of the optic on the data. Raises when cannot.
val set: t<'S, 'F, 'G, 'T> -> value: 'G -> ('S -> 'T)
/// Sets the focuses of the optic on the data. Returns input when cannot.
val setDefault: t<'S, 'F, 'G, 'S> -> value: 'G -> ('S -> 'S)
/// Attempts to set the focuses of the optic on the data.
val trySet: t<'S, 'F, 'G, 'T> -> value: 'G -> ('S -> option<'T>)

/// Determines whether the focuses of the optic on the data can be removed.
val canRemove: t<'S, 'F, 'G, 'T> -> ('S -> bool)
/// Removes the focuses of the optic on the data. Raises when cannot.
val remove: t<'S, 'F, 'G, 'T> -> ('S -> 'T)
/// Removes the focuses of the optic on the data. Returns input when cannot.
val removeDefault: t<'S, 'F, 'G, 'S> -> ('S -> 'S)
/// Attempts to remove the focuses of the optic on the data.
val tryRemove: t<'S, 'F, 'G, 'T> -> ('S -> option<'T>)

/// An optic that operates on the focuses in both ways.
val andAlso: second: t<'U, 'F, 'G, 'T>
          ->  first: t<'S, 'F, 'G, 'U>
          ->         t<'S, 'F, 'G, 'T>

/// Acts like the primary when it can view and otherwise like secondary.
val orElse: secondary: t<'S, 'F, 'G, 'T>
         ->   primary: t<'S, 'F, 'G, 'T>
         ->            t<'S, 'F, 'G, 'T>

/// Folds over the focuses of the optic on the data.
val fold: zero: 'R -> plus: ('R -> 'F -> 'R) -> t<'S, 'F, 'G, 'T> -> 'S -> 'R

/// Counts the number of focuses the optic has on the data.
val count: t<'S, 'F, 'G, 'T> -> ('S -> int)

/// Determines whether any focus of the optic on the data satisfy predicate.
val exists: predicate: ('F -> bool) -> t<'S, 'F, 'F, 'T> -> ('S -> bool)
/// Determines whether all focuces of the optic on the data satisfy predicate.
val forall: predicate: ('F -> bool) -> t<'S, 'F, 'F, 'T> -> ('S -> bool)

/// Iterates over the focuses of the optic on the data.
val iter: t<'S, 'F, 'G, 'T> -> action: ('F -> unit) -> ('S -> unit)

/// Extracts an array of all the focuses of the optic on the data.
val collect: t<'S, 'F, 'G, 'T> -> 'S -> IROL<'F>

/// Injects list elements to the focuses or removes them. Raises when cannot.
val disperse:     t<'S, 'F, 'G, 'T> -> values: #IROL<'G> -> ('S -> 'T)
/// Injects list elements to the focuses or keeps them.
val disperseKeep: t<'S, 'F, 'F, 'T> -> values: #IROL<'F> -> ('S -> 'T)

/// Converts an optic to a lens focusing on an array of the focuses.
val partsOf:     t<'S, 'F, 'G, 'T> -> t<'S, IROL<'F>, #IROL<'G>, 'T>
/// Converts an optic to a lens focusing on an array of the focuses.
val partsOfKeep: t<'S, 'F, 'F, 'T> -> t<'S, IROL<'F>, #IROL<'F>, 'T>

/// Defines a new lens from a getter and setter.
val lens: project: ('S -> 'F) -> inject: ('G -> 'S -> 'T) -> t<'S, 'F, 'G, 'T>

/// Defines a new isomorphism from a pair of conversion functions.
val iso: forward: ('S -> 'F) -> backward: ('G -> 'T) -> t<'S, 'F, 'G, 'T>

/// Defines a new prism from a constructor-destructor pair.
val prism: cons: ('G -> 'T) -> dest: ('S -> Choice<'T, 'F>) -> t<'S, 'F, 'G, 'T>

/// Defines a lens out of a suitable fold and a traversal.
val foldLens: aFold: (t<'S, 'E, 'G, 'T> -> ('S -> 'F))
           -> aTraversal: t<'S, 'E, 'G, 'T>
           -> t<'S, 'F, 'G, 'T>

/// Views through isomorphism in inverse direction. Raises on non-isos.
val review: anIso: t<'S, 'F, 'G, 'T> -> ('G -> 'T)

/// Inverts the given isomorphism. Raises on non-isos.
val invertI: anIso: t<'S, 'F, 'G, 'T> -> t<'G, 'T, 'S, 'F>

/// A prism with a focus only when it passes the given predicate.
val whereP: predicate: ('F -> bool) -> t<'F>

/// A non-isomorphism computed from the focus.
val choose: toOptic: ('S -> t<'S, 'F, 'G, 'T>) -> t<'S, 'F, 'G, 'T>

/// A choice between two non-isomorphisms depending on the focus.
val ifElse: predicate: ('S -> bool)
         -> onTrue:  t<'S, 'F, 'G, 'T>
         -> onFalse: t<'S, 'F, 'G, 'T>
         ->          t<'S, 'F, 'G, 'T>

/// A prism that never has a focus.
val zeroP: t<'S, 'F, 'G, 'S>

/// A prism that never has a focus and signals removal of parent on over.
val removeP: t<'S, 'F, 'G, 'T>

/// A lens like optic that signals removal when written value matches predicate.
val removeIfL: predicate: ('G -> bool) -> t<'F, 'F, 'G, 'G>
/// A lens like optic that signals removal when written with equal value.
val removeEqL: value: 'G -> t<'F, 'F, 'G, 'G> when 'G: equality

/// A prism that peels away `Some` and removes `None`.
val optionP: t<option<'F>, 'F, 'G, 'G>

/// A lens like optic that maps a removed focus to `None` when written.
val removeAsNoneL: t<'F, 'F, 'G, option<'G>>
/// A lens like optic that maps `None` to a removed focus when written.
val noneAsRemoveL: t<'F, 'F, option<'G>, 'G>

/// A prism that focuses on `Some` value of optional if any. Removable.
val someP: t<option<'F>, 'F, 'G, option<'G>>

/// An isomorphism between options.
val optionI: t<       'S ,        'F ,        'G ,        'T >
          -> t<option<'S>, option<'F>, option<'G>, option<'T>>

val toDefaultI: value: 'F -> t<option<'F>, 'F, 'G, 'G> when 'F: equality
val ofDefaultI: value: 'G -> t<'F, 'F, 'G, option<'G>> when 'G: equality
val defaultI: value: 'F -> t<option<'F>, 'F, 'F, option<'F>> when 'F: equality

/// A prism that focuses on `Choice1Of2`.
val choice1of2P: t<Choice<'F, 'S>, 'F, 'G, Choice<'G, 'S>>
/// A prism that focuses on `Choice2Of2`.
val choice2of2P: t<Choice<'S, 'F>, 'F, 'G, Choice<'S, 'G>>

/// The identity isomorphism.
val idI: t<'S, 'S, 'T, 'T>

/// An identity like optic that performs a given side-effect when viewed.
val beforeL: action: ('S -> unit) -> t<'S, 'S, 'T, 'T>

/// An isomorphism that maps the focus with given function when read.
val rereadI: forward: ('S -> 'F) -> t<'S, 'F, 'T, 'T>
/// An isomorphism that maps the focus with given function when written.
val rewriteI: backward: ('G -> 'T) -> t<'S, 'S, 'G, 'T>
/// An isomorphism that maps the focus with given function in both ways.
val normalizeI: ward: ('S -> 'F) -> t<'S, 'F, 'S, 'F>

/// A lens focusing on the first element of a pair.
val fstL: t<'L1 * 'R, 'L1, 'L2, 'L2 * 'R>
/// A lens focusing on the first element of a struct pair.
val fstsL: t<struct ('L1 * 'R), 'L1, 'L2, struct ('L2 * 'R)>
/// A lens focusing on the second element of a pair.
val sndL: t<'L * 'R1, 'R1, 'R2, 'L * 'R2>
/// A lens focusing on the second element of a struct pair.
val sndsL: t<struct ('L * 'R1), 'R1, 'R2, struct ('L * 'R2)>
/// An isomorphism between pairs.
val pairI: fstIso: t<'SL, 'FL, 'GL, 'TL>
        -> sndIso: t<'SR, 'FR, 'GR, 'TR>
        ->         t<'SL * 'SR, 'FL * 'FR, 'GL * 'GR, 'TL * 'TR>
/// An isomorphism between struct pairs.
val pairsI: fstIso: t<'SL, 'FL, 'GL, 'TL>
         -> sndIso: t<'SR, 'FR, 'GR, 'TR>
         -> t<struct ('SL * 'SR),
              struct ('FL * 'FR),
              struct ('GL * 'GR),
              struct ('TL * 'TR)>
/// A lens focusing an a pair.  Given optics should be separable lenses.
val pairL: fstLens: t<'S, 'L1,       'L2,       'S>
        -> sndLens: t<'S,       'R1,       'R2, 'T>
        ->          t<'S, 'L1 * 'R1, 'L2 * 'R2, 'T>
/// A lens focusing an a struct pair.  Given optics should be separable lenses.
val pairsL: fstLens: t<'S,         'L1       ,         'L2       , 'S>
         -> sndLens: t<'S,               'R1 ,               'R2 , 'T>
         ->          t<'S, struct ('L1 * 'R1), struct ('L2 * 'R2), 'T>
/// An isomorphism between struct pair and (class) pair.
val cpairI: t<struct ('L1 * 'R1), 'L1 * 'R1, 'L2 * 'R2, struct ('L2 * 'R2)>
/// An isomorphism between (class) pair and struct pair.
val spairI: t<'L1 * 'R1, struct ('L1 * 'R1), struct ('L2 * 'R2), 'L2 * 'R2>

/// An isomorphism between integers and floats.
val truncateI: t<int, float>

/// An isomorphism between arrays and read only lists.
val arrayI: t<#IROL<'F>, 'F[], 'G[], IROL<'G>>
/// An isomorphism between read only lists and arrays.
val rolistI: t<'F[], IROL<'F>, #IROL<'G>, 'G[]>

/// A prism focusing on element at given index of a list. Removable.
val atP:    index:        int  -> t<#IROL<'F>, 'F, 'F, IROL<'F>>
/// A prism focusing on element at a mutable index of a list. Removable.
val atRefP: indexRef: ref<int> -> t<#IROL<'F>, 'F, 'F, IROL<'F>>

/// A lens like optic focusing on an element that matches the given predicate.
val findL: ('F -> bool) -> t<#IROL<'F>, option<'F>, 'F, IROL<'F>>

/// A prism focusing on an element that matches the given predicate.
val findP: ('F -> bool) -> t<#IROL<'F>, 'F, 'F, IROL<'F>>

/// An isomorphism between given values and booleans.  Only truthy maps to true.
val isOrI: falsy: 'F -> truthy: 'F -> t<'F, bool> when 'F: equality

/// A lens like optic focusing on whether list contains given element.
val containsL: 'F -> t<#IROL<'F>, bool, bool, IROL<'F>> when 'F: equality

/// A traversal over the elements of a list. Removable.
val elemsT: t<#IROL<'F>, 'F, 'G, IROL<'G>>
/// An isomorphism between lists.
val elemsI: t<'S, 'F, 'G, 'T> -> t<#IROL<'S>, IROL<'F>, #IROL<'G>, IROL<'T>>

/// Traversal limited to specified subsequence of focuses.
val subT: offset: int -> count: int -> t<'S, 'F, 'F, 'T> -> t<'S, 'F, 'F, 'T>
/// Traversal limited to focuses remaining after specified count.
val dropT: count: int -> (t<'S, 'F, 'F, 'T> -> t<'S, 'F, 'F, 'T>)
/// Traversal limited to specified number of focuses.
val takeT: count: int -> (t<'S, 'F, 'F, 'T> -> t<'S, 'F, 'F, 'T>)

/// An indexed traversal.
val indexedT: t<'S, 'F, 'G, 'T> -> t<'S, struct (int * 'F), 'G, 'T>

/// An isomorphism that partitions a list into sublists of passes and fails.
val partitionI: predicate: ('F -> bool)
          -> t<#IROL<'F>,
               struct (IROL<'F> * IROL<'F>),
               struct (#IROL<'G> * #IROL<'G>),
               IROL<'G>>
/// A lens that focuses on sublist of passes.
val filterL: predicate: ('F -> bool)
          -> t<#IROL<'F>, IROL<'F>, #IROL<'F>, IROL<'F>>
/// A lens that focuses on sublist of fails.
val rejectL: predicate: ('F -> bool)
          -> t<#IROL<'F>, IROL<'F>, #IROL<'F>, IROL<'F>>

/// An isomorphism that splits a list into two sublists at given position.
val splitAtI: relative: int
          -> t<#IROL<'F>,
               struct (IROL<'F> * IROL<'F>),
               struct (#IROL<'G> * #IROL<'G>),
               IROL<'G>>
/// A lens reads empty and prepends when written.
val prependL: t<#IROL<'F>, IROL<'F>, #IROL<'F>, IROL<'F>>
/// A lens that reads empty and appends when written.
val appendL: t<#IROL<'F>, IROL<'F>, #IROL<'F>, IROL<'F>>

/// An isomorphism whose focus is reverse of the list.
val revI: t<#IROL<'F>, IROL<'F>, #IROL<'G>, IROL<'G>>

/// An isomorphism between a list and an indexed list.
val indexedI: t<#IROL<'F>,
                IROL<struct (int * 'F)>,
                #IROL<struct (int * 'G)>,
                IROL<'G>>

/// An isomorphism between string with separators and the separated strings.
val splitI: separator: string -> t<string, IROL<string>, #IROL<string>, string>

/// An isomorphism between strings with and without a prefix.
val dropPrefixI: prefix: string -> t<string, option<string>, string, string>

/// An isomorphism between strings with substrings replaced.
val replaceI: inn: string -> out: string -> t<string>

/// An isomorphism between values and values that pass the predicate.
val subsetI: predicate: ('S -> bool) -> t<'S, option<'S>, 'T, 'T>

/// An isomorphism between strings with an optional separator and pairs.
val uncoupleI: separator: string -> t<string, struct (string * string)>

/// An isomorphism between URL encoded and plain strings.
val urlDecodeI: t<string>
/// An isomorphism between plain and URL encoded strings.
val urlEncodeI: t<string>

/// An isomorphism between list of key-value pairs and map of keys to values.
val toMultiMapI: t<#IROL<struct ('FK * 'FV)>,
                   Map<'FK, IROL<'FV>>,
                   Map<'GK, #IROL<'GV>>,
                   IROL<struct ('GK * 'GV)>>
                 when 'FK: comparison and 'GK: comparison
/// An isomorphism between map of keys to values and list of key-value pairs.
val ofMultiMapI: t<Map<'FK, #IROL<'FV>>,
                   IROL<struct ('FK * 'FV)>,
                   #IROL<struct ('GK * 'GV)>,
                   Map<'GK, IROL<'GV>>>
                 when 'FK: comparison and 'GK: comparison

/// An isomorphism between querystrings and maps of keys to values.
val querystringI: t<string, Map<string, IROL<string>>>
