//! The actual functions with which we can reconstruct a function in AST-aligned form.

/// A return statement (or final expression without a semicolon).
#[inline]
pub fn rtn<T>(x: T) -> T {
    x
}

/// A variable identified by a path through its enclosing modules.
#[inline]
pub fn path<T>(x: T) -> T {
    x
}

/// A literal, like `true`, `3`, `3.14159`, etc.
#[inline]
pub fn literal<T>(x: T) -> T {
    x
}

/// Addition.
#[inline]
pub fn add<A, B, C>(a: A, b: B) -> C
where
    A: std::ops::Add<B, Output = C>,
{
    a + b
}

/// Subtraction.
#[inline]
pub fn sub<A, B, C>(a: A, b: B) -> C
where
    A: std::ops::Sub<B, Output = C>,
{
    a - b
}
