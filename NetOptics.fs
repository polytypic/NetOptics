namespace NetOptics

/// Optics are functions composable with the standard `<<` operator.
type Optic<'S, 'F, 'G, 'T> = Optic.t<'S, 'F, 'G, 'T>

/// Optics are functions composable with the standard `<<` operator.
type Optic<'S, 'F> = Optic<'S, 'F, 'F, 'S>

/// Optics are functions composable with the standard `<<` operator.
type Optic<'S> = Optic<'S, 'S>

/// Short alias for the `IReadOnlyList` type.
type IROL<'T> = Optic.IROL<'T>

/// History collection type.
type History<'S> = History.t<'S>
