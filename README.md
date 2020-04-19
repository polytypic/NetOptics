# NetOptics

This is a prototype optics library for .NET in F# based on an approach that does
not require (an encoding of) higher-kinded polymorphism or ad-hoc polymorphism
and can also be implemented and used in C# (in which the initial work was done)
and other languages with similar features (first-class functions, rank-1
polymorphism, mutable objects, ability to create a default value of any type).

**NOTE:** I have no plans to productize this as a proper library.

## Overview

See the [NetOptics.fsi](NetOptics.fsi) signature.

**Features:**

- Simple types
- Optics are just functions and can be composed with the standard function
  composition operator `<<`
- Isomorphisms (or adapters) are invertible
- Lenses
- Prisms
- Folds
- Traversals
- Ability to remove focuses of an optic

**Not supported:**

- Polymorphic update
- Applicative traverse over focuses of an optic

**NOTE:** An alternative design could support polymorphic update with four type
parameters and my initial implementation in C# actually did. Unfortunately there
is no way to define parameterized type aliases in C#, so I quickly dropped to
monomorphic optics to avoid verbosity that was not strictly required for my use
case.

### On type safety

The approach in this prototype is mostly type-safe. However, the number of
focuses or the class of an optic is not tracked in the types.

For example, the `view` operation requires the given optic to have at least one
focus on the target data structure. If there are no focuses, `view` will raise
an exception. Likewise, the `invertI` and `review` operations require an
(explicitly constructed) isomorphism or composition thereof. If the given optic
is not an explicitly constructed isomorphism, full application of `invertI` or
`review` will raise an exception.

**NOTE:** I have not explored a design that would use phantom types to track all
the details required to avoid exceptions in the course of prototyping this
library, but based on previous experience I'm confident that it can be done.

### On performance

This approach admits an implementation that does not perform large numbers of
allocations when an optic is used. This means that performance should be good.
However, limited inlining in .NET and F# will put a cap on performance.

## On the implementation

Here I will describe the [internal implementation](NetOptics.fs) briefly for the
interest of people who might want to implement this approach in their language.
This description is just a hint. Read the [code](NetOptics.fs) for full details.

The [signature](NetOptics.fsi) of this library reveals that `Optic<'s, 'a>` is a
function type, but does not expose the implementation of the `Pipe<'a>` type:

```fsharp
type Pipe<'a>

type Optic<'s,'a> = Pipe<'a> -> Pipe<'s>
```

Here is how the `Pipe<'a>` type is defined internally:

```fsharp
type [<Struct>] Context =
  val mutable Over: bool
  val mutable Hit: bool
  val mutable View: obj

type D<'a> = delegate of byref<Context> * 'a -> 'a

type Pipe<'a> = P of D<'a> * inverted: bool
```

Now we can see that an optic is a function that takes a mapping function with a
mutable context to a another mapping function with a context.

**NOTE:** The `D<'a>` type is an explicit `delegate` type and is required in
.NET to use `byref<_>` arguments. The use of `byref<_>` is an optimization to
avoid a heap allocation for the context. In other languages (and also in .NET)
you could also just use a function type taking a mutable context object (with
the cost of an extra allocation in .NET).

The `inverted` flag of the `Pipe<_>` type is used to track whether an
isomorphism is being inverted by use of `review`. Isomorphisms propagate the
flag during construction, but other optics set it to `false`. This allows
`review` to check that inversion works and allows an isomorphism to avoid
performing potentially illegal operations on invalid data. During an inverse
operation the input data given to the pipe is undefined (an
`Unchecked.defaultof<_>`).

The context is used by operations on optics, like `view` and `over`, to
communicate with the optic being operated upon and to determine the result of
the operation. The context is created just before the optic is used and does not
escape the use. This means that outside of the internal mechanism one cannot
observe the use of mutation and operations can be referentially transparent.

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
