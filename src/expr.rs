//! Nodes in an AST-aligned representation.

/// Anything that can serve as an AST node and do everything we need it to.
pub trait Expr {
    /// Trivial measure of complexity: a variable is 1, an operation is 1, a binary op is 2, a conditional is 3, etc.
    const COMPLEXITY: usize;
    /// Find and return a mutation, if any, that passes the given test suite.
    fn mutate<F, C: Fn(F) -> Option<&'static str>>(&self, _check: C) -> Option<&'static str>;
}

/// A block of statements: `{ ... }`
pub struct Block<T: StatementNode>(pub T);
impl<T: StatementNode> Expr for Block<T> {
    const COMPLEXITY: usize = T::COMPLEXITY;
    fn mutate<F, C: Fn(F) -> Option<&'static str>>(&self, _check: C) -> Option<&'static str> {
        Some("unimplemented")
    }
}

/// Haskell-like "list" elements (either the end of the list or a cons).
pub trait StatementNode: Expr {}

/// Delimiter at the end of a "list" of statements, like `...:[]` in Haskell.
pub struct EndOfBlock;
impl Expr for EndOfBlock {
    const COMPLEXITY: usize = 0;
    fn mutate<F, C: Fn(F) -> Option<&'static str>>(&self, _check: C) -> Option<&'static str> {
        Some("unimplemented")
    }
}
impl StatementNode for EndOfBlock {}

/// Wrapper around a statement potentially followed by another statement.
pub struct StatementList<S: Expr, NextStatement: StatementNode>(pub S, pub NextStatement);
impl<S: Expr, NextStatement: StatementNode> Expr for StatementList<S, NextStatement> {
    const COMPLEXITY: usize = S::COMPLEXITY + NextStatement::COMPLEXITY;
    fn mutate<F, C: Fn(F) -> Option<&'static str>>(&self, _check: C) -> Option<&'static str> {
        Some("unimplemented")
    }
}
impl<S: Expr, NextStatement: StatementNode> StatementNode for StatementList<S, NextStatement> {}

/// Statement, like `x += 3;`.
pub struct Statement;

/// A literal, like `true`, `3`, `3.14159`, ...
pub struct Literal<T>(T);
impl<T> Expr for Literal<T> {
    const COMPLEXITY: usize = 1;
    fn mutate<F, C: Fn(F) -> Option<&'static str>>(&self, _check: C) -> Option<&'static str> {
        Some("unimplemented")
    }
}

//%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
