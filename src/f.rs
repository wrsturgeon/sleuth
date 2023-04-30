//! The actual functions with which we can reconstruct a function in AST-aligned form.

/// A return statement (or final expression without a semicolon).
#[inline]
pub const fn rtn<T>(x: T) -> T {
    x
}

/// A variable identified by a path through its enclosing modules.
#[inline]
pub const fn path<T>(x: T) -> T {
    x
}

/// A literal, like `true`, `3`, `3.14159`, etc.
#[inline]
pub const fn literal<T>(x: T) -> T {
    x
}

/// Addition.
#[inline]
pub fn add<A, B, C>(a: A, b: B) -> C
where
    A: core::ops::Add<B, Output = C>,
{
    #![allow(clippy::arithmetic_side_effects)]
    a + b
}

/// Subtraction.
#[inline]
pub fn sub<A, B, C>(a: A, b: B) -> C
where
    A: core::ops::Sub<B, Output = C>,
{
    #![allow(clippy::arithmetic_side_effects)]
    a - b
}

/// Branch/conditional (i.e. `if`).
#[inline]
pub fn cond<T, A: Fn() -> T, B: Fn() -> T>(c: bool, then: A, otherwise: B) -> T {
    if c {
        then()
    } else {
        otherwise()
    }
}
