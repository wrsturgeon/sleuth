# Sleuth
Extremely opinionated testing framework generating an exact specification and reducing code to its minimal implementation.

## At a Glance
This library takes the idea behind mutation testing to its extreme.

Mutation testing believes that small errors in your source code (like `a > b` instead of `a >= b`) are likely errors; when they happen, your tests _should_ fail, and mutation testing frameworks make sure they do. When your test suite fails to recognize a bug, it means you need to add a new test.

Rust already has mutation testing frameworks which I admire and don't attempt to replace. This library is an experiment in mutating _all_ of your code, proving by brute force that no function shorter than the one you've written can meet your specifications. It should encourage test-driven development without the burden of writing an exhaustive test suite beforehand; after an initial attempt, your tests and your implementation will evolve together as you encounter all the ways something might be done and eventually narrow down to what you wanted all along.

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

#[sleuth(
    roundtrip(sub_one, 42),
    !roundtrip(add_one, 42),
)]
fn add_one(x: u8) -> u8 {
    x + 1
}

#[sleuth(
    roundtrip(add_one, 42),
    !roundtrip(sub_one, 42),
)]
fn sub_one(x: u8) -> u8 {
    x - 1
}
```
Note a few things:
- `roundtrip` returns a bool to indicate success or failure instead of `panic`king like a usual test.
- The `sleuth` attribute takes a function with its first argument missing (here, `roundtrip` without `f`), and that argument is filled in at compile time with the function the attribute is applied to (here, `add_one` and `sub_one`).
- We don't spam `roundtrip` with a bunch of random inputs (though you can do that separately!). Instead, you choose your inputs (here, `42` both times), and the library helps you find the minimal set that knocks out all mutations.

The library is still under development—I'm an undergrad with a chronic lack of free time—but currently, it can run a test suite of properties like above, and fairly soon it should be able to reduce common cases like the following:
```rust
fn is_true(b: bool) -> bool {
    if b { true } else { false }
}
```
to their minimal implementations:
```rust
fn is_true(b: bool) -> bool {
    b
}
```
which usual mutation testing could never (and doesn't aim to) fix. Clippy can currently fix trivial errors like these on a whitelisted basis, but this library aims to eliminate the whole class of errors from the bottom up rather than playing whack-a-mole.

## How it Works

Given a function like this,
```rust
#[sleuth(
    does_nothing(false),
    does_nothing(true),
)]
const fn is_true(b: bool) -> bool {
    if b { true } else { false }
}
```
the `sleuth` attribute rewrites it at compile time as the following:
```rust
const fn is_true(b: bool) -> bool {
    if b { true } else { false }
}

#[cfg(test)]
mod is_true_sleuth {
    use super::*;

    // A struct with each scoped variable.
    struct Scope { pub b: bool }

    // A fully implemented AST node for each function argument.
    mod scoped_variables {
        pub struct B; // CamelCase by convention but referring to the argument `b`
        impl ::sleuth::Expr for B {
            type Return = bool;
            type Scope = super::Scope;
            const COMPLEXITY: usize = 1;
            fn eval(&self, scope: &mut Self::Scope) -> Self::Return {
                scope.b
            }
        }
    }

    // Instantiation of a unique type for the exact AST of `is_true`.
    type Ast = ::sleuth::expr::{... many lines, lots of generics ...};
    const AST: Ast = ::sleuth::expr::{... instantiation of the above ...};

    // Checks, for any function, whether it fulfills the test suite for `is_true`.
    pub fn check<_FnToCheck>(f: &_FnToCheck) -> Option<&'static str>
      where
        _FnToCheck: Fn(bool) -> bool + ::core::panic::RefUnwindSafe
    {
        match std::panic::catch_unwind(|| crate::sleuth::does_nothing(f, false)) {
            Ok(b) => if !b { Some("crate::sleuth::does_nothing(f, false)") } else { None },
            _ => Some("crate::sleuth::does_nothing(f, false) PANICKED (see two lines above)"),
        }
            .or_else(|| match std::panic::catch_unwind(|| crate::sleuth::does_nothing(f, true)) {
                Ok(b) => if !b { Some("crate::sleuth::does_nothing(f, true)") } else { None },
                _ => Some("crate::sleuth::does_nothing(f, true) PANICKED (see two lines above)"),
            })
    }

    #[test]
    fn test_original() {
        // check that the original function passes before mutating
        ::sleuth::testify(check(&is_true))
    }
    
    #[test]
    fn test_mutants() {
        use ::sleuth::Expr;

        // If there's something wrong with our AST, pass and don't mutate
        // And if it's an error in your logic, `test_original` won't pass
        if check(&(|b| AST.eval(&mut Scope { b }))).is_some() { return; }

        // breadth-first search
        for mutation_severity in 0usize..=AST::COMPLEXITY {
            // . . .
            // very long, not yet complete
            // . . .
            // eventually, with a different value in place of `AST` each time:
            ::sleuth::testify(check(&(|b| AST.eval(&mut Scope { b }))))
        }
    }
}
```
You can view this expansion by running `EXPAND=1 cargo expand`. The `EXPAND` environment variable turns `#[cfg(test)]` and `#[test]` into nonsense so that `cargo expand` doesn't assume we're not testing and delete the whole module.

Note that unless we're testing, the macro has __no effect__. It should be safe to use even in production.
