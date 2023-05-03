//! Nodes in an AST-aligned representation.

#![allow(clippy::inline_always)] // AST nodes shouldn't cause any runtime overhead

use core::marker::PhantomData;

/// Anything that can serve as an AST node and do everything we need it to.
pub trait Expr {
    /// Return type of `eval` (if not an error).
    type Return;
    /// Data structure holding all variables in scope.
    type Scope;
    /// Trivial measure of complexity: a variable is 1, an operation is 1, a binary op is 2, a conditional is 3, etc.
    const COMPLEXITY: usize;
    /// Find and return a mutation, if any, that passes the given test suite.
    // fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
    //     &self,
    //     check: C,
    //     example_fn: &F,
    // ) -> Option<&'static str>;
    /// Evaluate as if running the original function.
    fn eval(&self, scope: &mut Self::Scope) -> Self::Return;
}

/// A block of statements: `{ ... }`
pub struct Block<T: StatementNode<Scope = Scope>, Scope>(pub T, pub PhantomData<Scope>);
impl<T: StatementNode<Scope = Scope>, Scope> Expr for Block<T, Scope> {
    type Return = T::Return;
    type Scope = Scope;
    const COMPLEXITY: usize = 1 + T::COMPLEXITY;
    #[inline(always)]
    fn eval(&self, scope: &mut Self::Scope) -> Self::Return {
        self.0.eval(scope)
    }
}

/// Haskell-like "list" elements (either the end of the list or a cons).
pub trait StatementNode: Expr {}

/// Delimiter at the end of a "list" of statements, like `...:[]` in Haskell. **Necessary because of empty blocks.**
pub struct EndList<Scope>(PhantomData<Scope>);
impl<Scope> Expr for EndList<Scope> {
    type Return = ();
    type Scope = Scope;
    const COMPLEXITY: usize = 0;
    #[inline(always)]
    fn eval(&self, _: &mut Self::Scope) -> Self::Return {}
}
impl<Scope> StatementNode for EndList<Scope> {}

/// Wrapper around a statement potentially followed by another statement.
pub struct StatementList<
    S: Expr<Return = (), Scope = Scope>,
    NextStatement: StatementNode<Scope = Scope>,
    Scope,
>(pub S, pub NextStatement, pub PhantomData<Scope>);
impl<S: Expr<Return = (), Scope = Scope>, NextStatement: StatementNode<Scope = Scope>, Scope> Expr
    for StatementList<S, NextStatement, Scope>
{
    type Return = NextStatement::Return;
    type Scope = Scope;
    const COMPLEXITY: usize = S::COMPLEXITY + NextStatement::COMPLEXITY;
    #[inline(always)]
    fn eval(&self, scope: &mut Self::Scope) -> Self::Return {
        self.0.eval(scope);
        self.1.eval(scope)
    }
}
impl<S: Expr<Return = (), Scope = Scope>, NextStatement: StatementNode<Scope = Scope>, Scope>
    StatementNode for StatementList<S, NextStatement, Scope>
{
}

/// Wrapper around the last statement in a block without a semicolon, locally returning its value.
pub struct ImplicitReturn<S: Expr<Scope = Scope>, Scope>(pub S, pub PhantomData<Scope>);
impl<S: Expr<Scope = Scope>, Scope> Expr for ImplicitReturn<S, Scope> {
    type Return = S::Return;
    type Scope = Scope;
    const COMPLEXITY: usize = S::COMPLEXITY;
    #[inline]
    fn eval(&self, scope: &mut Self::Scope) -> Self::Return {
        self.0.eval(scope)
    }
}
impl<S: Expr<Scope = Scope>, Scope> StatementNode for ImplicitReturn<S, Scope> {}

/// A literal, like `true`, `3`, `3.14159`, ...
pub struct Literal<T: Copy /* does NOT have to impl Expr */, Scope>(pub T, pub PhantomData<Scope>);
impl<T: Copy, Scope> Expr for Literal<T, Scope> {
    type Return = T;
    type Scope = Scope;
    const COMPLEXITY: usize = 0; // TODO: controversial
    #[inline(always)]
    fn eval(&self, _: &mut Self::Scope) -> Self::Return {
        self.0
    }
}

/// An `if` statement without an `else` block.
pub struct If<
    Cond: Expr<Return = bool, Scope = Scope>,
    Left: Expr<Return = (), Scope = Scope>,
    Scope,
>(pub Cond, pub Left, pub PhantomData<Scope>);
impl<Cond: Expr<Return = bool, Scope = Scope>, Left: Expr<Return = (), Scope = Scope>, Scope> Expr
    for If<Cond, Left, Scope>
{
    type Return = Left::Return;
    type Scope = Scope;
    const COMPLEXITY: usize = 1 + Cond::COMPLEXITY + Left::COMPLEXITY;
    #[inline(always)]
    fn eval(&self, scope: &mut Self::Scope) -> Self::Return {
        if self.0.eval(scope) {
            self.1.eval(scope);
        }
    }
}

/// An `if` statement with an `else` block.
pub struct IfElse<
    Cond: Expr<Return = bool, Scope = Scope>,
    Left: Expr<Scope = Scope>,
    Right: Expr<Return = Left::Return, Scope = Scope>,
    Scope,
>(pub Cond, pub Left, pub Right, pub PhantomData<Scope>);
impl<
        Cond: Expr<Return = bool, Scope = Scope>,
        Left: Expr<Scope = Scope>,
        Right: Expr<Return = Left::Return, Scope = Scope>,
        Scope,
    > Expr for IfElse<Cond, Left, Right, Scope>
{
    type Return = Left::Return;
    type Scope = Scope;
    const COMPLEXITY: usize = 1 + Cond::COMPLEXITY + Left::COMPLEXITY + Right::COMPLEXITY;
    #[inline(always)]
    fn eval(&self, scope: &mut Self::Scope) -> Self::Return {
        if self.0.eval(scope) {
            self.1.eval(scope)
        } else {
            self.2.eval(scope)
        }
    }
}

/// Addition, e.g. `a + b`.
pub struct Add<Left: Expr<Scope = Scope>, Right: Expr<Scope = Scope>, Scope>(
    pub Left,
    pub Right,
    pub PhantomData<Scope>,
)
where
    Left::Return: core::ops::Add<Right::Return>;
impl<Left: Expr<Scope = Scope>, Right: Expr<Scope = Scope>, Scope> Expr for Add<Left, Right, Scope>
where
    Left::Return: core::ops::Add<Right::Return>,
{
    type Return = <Left::Return as core::ops::Add<Right::Return>>::Output;
    type Scope = Scope;
    const COMPLEXITY: usize = 1 + Left::COMPLEXITY + Right::COMPLEXITY;
    #[inline(always)]
    fn eval(&self, scope: &mut Self::Scope) -> Self::Return {
        #![allow(clippy::arithmetic_side_effects)]
        self.0.eval(scope) + self.1.eval(scope)
    }
}

/// Subtraction, e.g. `a - b`.
pub struct Sub<Left: Expr<Scope = Scope>, Right: Expr<Scope = Scope>, Scope>(
    pub Left,
    pub Right,
    pub PhantomData<Scope>,
)
where
    Left::Return: core::ops::Sub<Right::Return>;
impl<Left: Expr<Scope = Scope>, Right: Expr<Scope = Scope>, Scope> Expr for Sub<Left, Right, Scope>
where
    Left::Return: core::ops::Sub<Right::Return>,
{
    type Return = <Left::Return as core::ops::Sub<Right::Return>>::Output;
    type Scope = Scope;
    const COMPLEXITY: usize = 1 + Left::COMPLEXITY + Right::COMPLEXITY;
    #[inline(always)]
    fn eval(&self, scope: &mut Self::Scope) -> Self::Return {
        #![allow(clippy::arithmetic_side_effects)]
        self.0.eval(scope) - self.1.eval(scope)
    }
}
