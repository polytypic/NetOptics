# NetOptics

This is a prototype optics library for .NET in F# based on an approach that does
not require (an encoding of) higher-kinded polymorphism or ad-hoc polymorphism
and can also be implemented and used in C# (in which the initial work was done)
and other languages with similar features (first-class functions, rank-1
polymorphism, mutable objects, ability to create a default value of any type).

**NOTE:** I have no plans to productize this as a proper library.

## Overview

Please study the [NetOptics.Optic.fsi](NetOptics.Optic.fsi) signature.

**Features:**

- Simple polymorphic types
- Optics are just functions and can be composed with the standard function
  composition operator `<<`
- Isomorphisms (or adapters) are invertible
- Lenses
- Prisms
- Folds
- Traversals
- Ability to remove focuses of an optic

**Not supported:**

- Applicative traverse over focuses of an optic

### On type safety

The approach in this prototype is mostly type-safe. However, the number of
focuses or the class of an optic is not tracked in the types.

For example, the `view` operation requires the given optic to have at least one
focus on the target data structure. If there are no focuses, `view` will raise
an exception. Likewise, the `invertI` combinator and the `review` operation
require an (explicitly constructed) isomorphism or composition thereof. If the
given optic is not an explicitly constructed isomorphism, full application of
`invertI` or `review` will raise an exception.

**NOTE:** I have not explored a design that would use phantom types to track all
the details required to avoid exceptions in the course of prototyping this
library, but based on previous experience I'm confident that it can be done.

### On performance

This approach admits an implementation that does not perform large numbers of
allocations when an optic is used. This means that performance should be good.
However, limited inlining in .NET and F# will put a cap on performance.

## On the implementation

Here I will describe the [internal implementation](NetOptics.Optic.fs) briefly
for the interest of people who might want to implement this approach in their
language. This description is just a hint. Read the [code](NetOptics.Optic.fs)
for full details.

The [signature](NetOptics.Optic.fsi) of this library reveals that the optic type
`t<'S, 'F, 'G, 'T>` is a function type, but does not expose the implementation
of the `Pipe<'S, 'T>` type:

```fsharp
type Pipe<'S, 'T>

type t<'S, 'F, 'G, 'T> = Pipe<'F, 'G> -> Pipe<'S, 'T>
```

Here is how the `Pipe<'S, 'T>` type is defined internally:

```fsharp
type [<Struct>] Context =
  val mutable Hit: bool
  val mutable Over: bool
  val mutable Index: int
  val mutable View: obj

type D<'S, 'T> = delegate of byref<Context> * 'S -> 'T

type Pipe<'S, 'T> = P of D<'S, 'T> * inverted: bool
```

Now we can see that an optic is a function that takes a mapping function with a
mutable context to a another mapping function with a mutable context.

**NOTE:** The `D<'S, 'T>` type is an explicit `delegate` type and is required in
.NET to use `byref<_>` arguments. The use of `byref<_>` is an optimization to
avoid a heap allocation for the context. In other languages (and also in .NET)
you could also just use a function type taking a mutable context object (with
the cost of an extra allocation in .NET).

The `inverted` flag of the `Pipe<'S, 'T>` type is used to track whether an
isomorphism is being inverted by use of `review`. Isomorphisms propagate the
flag during construction, but other optics set it to `false`. This allows
`review` to check that inversion works and allows an isomorphism to avoid
performing potentially illegal operations on invalid data. During an inverse
operation the input data given to the pipe is undefined (an
`Unchecked.defaultof<_>`).

The mutable context is used by operations on optics, like `view` and `over`, to
communicate with the optic being operated upon and to determine the result of
the operation. The context is created just before the optic is used and does not
escape the use. This means that outside of the internal mechanism one cannot
observe the use of mutation and operations can be referentially transparent. In
fact, the use of a mutable context is merely an optimization. If we would
instead define `Pipe<'S, 'T>` essentially as

```fsharp
type Pipe<'S, 'T> = Context * 'S -> Context * 'T
```

we could implement optics with same behaviour without using any mutation even in
a pure language like Haskell.
[Purity is an extensional property](https://eiriktsarpalis.wordpress.com/2017/03/06/f-and-purity/#comment-136).

The fields of the context are used roughly as follows:

- `Over` specifies whether or not an `over` or a `view` style operation is being
  performed. An `over` style operation must construct a return value, while a
  `view` style operation can avoid the construction and return anything that
  fits the type (like an `Unchecked.defaultof<_>`).

- `Hit` is used differently depending on `Over`:

  - A `view` style operation is stopped after `Hit` is set to `true`. No value
    can be returned when a `view` style operation ends with `Hit = false`.

  - During an `over` style operation, focuses that are `Hit` are removed. No
    value can be returned when an `over` style operation ends with `Hit = true`.

- `View` is an untyped placeholder for the result of simple `view` operations
  and is an optimization to avoid an allocation.

- `Index` is used by some of the operations and is an optimization to avoid an
  allocation.
