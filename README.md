# Sleuth
Extremely opinionated testing framework generating an exact specification and reducing code to its minimal implementation.

## At a Glance
This library takes the idea behind mutation testing to its extreme.

Mutation testing believes that small errors in your source code (like `a > b` instead of `a >= b`) are likely errors; when they happen, your tests _should_ fail, and mutation testing frameworks make sure they do. When your test suite fails to recognize a bug, it means you need to add a new test.

Rust already has mutation testing frameworks (like `mutagen`) which I admire and in no way attempt to replace. However, with relatively short functions, this library is an experiment in mutating _all_ of your code, proving by brute force that no function shorter than the one you've written can meet your specifications. This encourages test-driven development without the burden of writing an exhaustive test suite beforehand; after an initial attempt, your tests and your implementation evolve together as you encounter all the ways something might be done on the way to what you wanted all along.

The library also takes inspiration from property-based testing like Haskell's `QuickCheck`. Your tests can be written with reusable properties and told not to pass with `!`, like the following:
```rust
use ::sleuth::sleuth;

fn roundtrip<T, U, F, G>(f: F, g: G, x: T) -> bool
  where
    T: PartialEq + Clone,
    F: Fn(U) -> T,
    G: Fn(T) -> U,
{
    x.clone() == f(g(x))
}

#[sleuth(roundtrip(sub_one, 42), !roundtrip(add_one, 42))]
fn add_one(x: u8) -> u8 { x + 1 }

#[sleuth(roundtrip(add_one, 42), !roundtrip(sub_one, 42))]
fn sub_one(x: u8) -> u8 { x - 1 }
```
Note a few things:
- `roundtrip` returns a bool to indicate success or failure instead of `panic`king like a usual test.
- The `sleuth` attribute takes a function with its first argument missing (here, `roundtrip` without `f`), and that argument is filled in at compile time with the function the attribute is applied to (here, `add_one` and `sub_one`).
- We don't spam `roundtrip` with a bunch of random inputs (though you can do that separately!). Instead, you choose your inputs (here, `0` and `42`), and the library helps you find the minimal set that knocks out all mutations.

The library is still under development—I'm an undergrad with a chronic lack of free time—but currently, it can run a test suite of properties like above, and fairly soon it should be able to handle cases like the following:
```rust
fn is_true(b: bool) -> bool {
    if b { true } else { false }
}
```
should very quickly become
```rust
fn is_true(b: bool) -> bool {
    b
}
```
which usual mutation testing could never (and doesn't aim to) fix.
