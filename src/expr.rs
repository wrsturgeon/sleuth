//! Nodes in an AST-aligned representation.

#![allow(clippy::inline_always)] // AST nodes shouldn't cause any runtime overhead

// TODO: change Option<&'static str> to Result<SOME REPRESENTATION OF ALL VARIABLES IN SCOPE, Err wrapper around &'static str>

/// Anything that can serve as an AST node and do everything we need it to.
pub trait Expr {
    /// Return type of `eval` (if not an error).
    type Return;
    /// Trivial measure of complexity: a variable is 1, an operation is 1, a binary op is 2, a conditional is 3, etc.
    const COMPLEXITY: usize;
    /// Find and return a mutation, if any, that passes the given test suite.
    // fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
    //     &self,
    //     check: C,
    //     example_fn: &F,
    // ) -> Option<&'static str>;
    /// Evaluate as if running the original function.
    fn eval<Scope>(&self, scope: &mut Scope) -> Self::Return;
}

/// A block of statements: `{ ... }`
pub struct Block<T: StatementNode>(pub T);
impl<T: StatementNode> Expr for Block<T> {
    type Return = T::Return;
    const COMPLEXITY: usize = 1 + T::COMPLEXITY;
    #[inline(always)]
    fn eval<Scope>(&self, scope: &mut Scope) -> Self::Return {
        self.0.eval(scope)
    }
}

/// Haskell-like "list" elements (either the end of the list or a cons).
pub trait StatementNode: Expr {}

/// Delimiter at the end of a "list" of statements, like `...:[]` in Haskell.
pub struct EndList;
impl Expr for EndList {
    type Return = ();
    const COMPLEXITY: usize = 0;
    #[inline(always)]
    fn eval<Scope>(&self, _: &mut Scope) -> Self::Return {}
}
impl StatementNode for EndList {}

/// Wrapper around a statement potentially followed by another statement.
pub struct StatementList<S: Expr<Return = ()>, NextStatement: StatementNode>(
    pub S,
    pub NextStatement,
);
impl<S: Expr<Return = ()>, NextStatement: StatementNode> Expr for StatementList<S, NextStatement> {
    type Return = NextStatement::Return;
    const COMPLEXITY: usize = S::COMPLEXITY + NextStatement::COMPLEXITY;
    #[inline(always)]
    fn eval<Scope>(&self, scope: &mut Scope) -> Self::Return {
        self.0.eval(scope);
        self.1.eval(scope)
    }
}
impl<S: Expr<Return = ()>, NextStatement: StatementNode> StatementNode
    for StatementList<S, NextStatement>
{
}

/// Wrapper around the last statement in a block without a semicolon, locally returning its value.
pub struct LastStatement<S: Expr>(pub S);
impl<S: Expr> Expr for LastStatement<S> {
    type Return = S::Return;
    const COMPLEXITY: usize = S::COMPLEXITY;
    #[inline]
    fn eval<Scope>(&self, scope: &mut Scope) -> Self::Return {
        self.0.eval(scope)
    }
}
impl<S: Expr> StatementNode for LastStatement<S> {}

/// A literal, like `true`, `3`, `3.14159`, ...
pub struct Literal<T: Copy /* does NOT have to impl Expr */>(pub T);
impl<T: Copy> Expr for Literal<T> {
    type Return = T;
    const COMPLEXITY: usize = 0; // TODO: controversial
    #[inline(always)]
    fn eval<Scope>(&self, _: &mut Scope) -> Self::Return {
        self.0
    }
}

/// An `if` statement without an `else` block.
pub struct If<Cond: Expr<Return = bool>, Left: Expr<Return = ()>>(pub Cond, pub Left);
impl<Cond: Expr<Return = bool>, Left: Expr<Return = ()>> Expr for If<Cond, Left> {
    type Return = Left::Return;
    const COMPLEXITY: usize = 1 + Cond::COMPLEXITY + Left::COMPLEXITY;
    #[inline(always)]
    fn eval<Scope>(&self, scope: &mut Scope) -> Self::Return {
        if self.0.eval(scope) {
            self.1.eval(scope)
        }
    }
}

/// An `if` statement with an `else` block.
pub struct IfElse<Cond: Expr<Return = bool>, Left: Expr, Right: Expr<Return = Left::Return>>(
    pub Cond,
    pub Left,
    pub Right,
);
impl<Cond: Expr<Return = bool>, Left: Expr, Right: Expr<Return = Left::Return>> Expr
    for IfElse<Cond, Left, Right>
{
    type Return = Left::Return;
    const COMPLEXITY: usize = 1 + Cond::COMPLEXITY + Left::COMPLEXITY + Right::COMPLEXITY;
    #[inline(always)]
    fn eval<Scope>(&self, scope: &mut Scope) -> Self::Return {
        if self.0.eval(scope) {
            self.1.eval(scope)
        } else {
            self.2.eval(scope)
        }
    }
}

/// Addition, e.g. `a + b`.
pub struct Add<Left: Expr, Right: Expr>(pub Left, pub Right)
where
    Left::Return: std::ops::Add<Right::Return>;
impl<Left: Expr, Right: Expr> Expr for Add<Left, Right>
where
    Left::Return: std::ops::Add<Right::Return>,
{
    type Return = <Left::Return as std::ops::Add<Right::Return>>::Output;
    const COMPLEXITY: usize = 1 + Left::COMPLEXITY + Right::COMPLEXITY;
    #[inline(always)]
    fn eval<Scope>(&self, scope: &mut Scope) -> Self::Return {
        self.0.eval(scope) + self.1.eval(scope)
    }
}

/// Subtraction, e.g. `a - b`.
pub struct Sub<Left: Expr, Right: Expr>(pub Left, pub Right)
where
    Left::Return: std::ops::Sub<Right::Return>;
impl<Left: Expr, Right: Expr> Expr for Sub<Left, Right>
where
    Left::Return: std::ops::Sub<Right::Return>,
{
    type Return = <Left::Return as std::ops::Sub<Right::Return>>::Output;
    const COMPLEXITY: usize = 1 + Left::COMPLEXITY + Right::COMPLEXITY;
    #[inline(always)]
    fn eval<Scope>(&self, scope: &mut Scope) -> Self::Return {
        self.0.eval(scope) - self.1.eval(scope)
    }
}
