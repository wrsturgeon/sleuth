//! Nodes in an AST-aligned representation.

#![allow(unused_variables)]

/// Notes that mutation is not yet implemented for this type of AST node (with a file and line number).
macro_rules! mutate_todo {
    () => {
        Some(concat!(
            "AST mutation not yet implemented: ",
            file!(),
            ":",
            line!()
        ))
    };
}

// TODO: change Option<&'static str> to Result<SOME REPRESENTATION OF ALL VARIABLES IN SCOPE, Err wrapper around &'static str>

/// Anything that can serve as an AST node and do everything we need it to.
pub trait Expr {
    /// Trivial measure of complexity: a variable is 1, an operation is 1, a binary op is 2, a conditional is 3, etc.
    const COMPLEXITY: usize;
    /// Find and return a mutation, if any, that passes the given test suite.
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str>;
}

/// A block of statements: `{ ... }`
pub struct Block<T: StatementNode>(pub T);
impl<T: StatementNode> Expr for Block<T> {
    const COMPLEXITY: usize = 1 + T::COMPLEXITY;
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        self.0.mutate(check, example_fn)
    }
}

/// Haskell-like "list" elements (either the end of the list or a cons).
pub trait StatementNode: Expr {}

/// Delimiter at the end of a "list" of statements, like `...:[]` in Haskell.
pub struct EndList;
impl Expr for EndList {
    const COMPLEXITY: usize = 0;
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        None
    }
}
impl StatementNode for EndList {}

/// Wrapper around a statement potentially followed by another statement.
pub struct StatementList<S: Expr, NextStatement: StatementNode>(pub S, pub NextStatement);
impl<S: Expr, NextStatement: StatementNode> Expr for StatementList<S, NextStatement> {
    const COMPLEXITY: usize = S::COMPLEXITY + NextStatement::COMPLEXITY;
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        self.0
            .mutate(&check, example_fn)
            .or_else(|| self.1.mutate(&check, example_fn))
    }
}
impl<S: Expr, NextStatement: StatementNode> StatementNode for StatementList<S, NextStatement> {}

/// Statement, like `x += 3;`.
pub struct Statement<T: Expr>(pub T);
impl<T: Expr> Expr for Statement<T> {
    const COMPLEXITY: usize = 1 + T::COMPLEXITY;
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        self.0.mutate(check, example_fn)
    }
}

/// A literal, like `true`, `3`, `3.14159`, ...
pub struct Literal<T /* does NOT have to impl Expr */>(pub T);
impl<T> Expr for Literal<T> {
    const COMPLEXITY: usize = 0; // TODO: controversial
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        None // eventually, `self.0`
    }
}

/// A path (even a trivial one), like `x`, `crate::some_mod::some_other_mod::x`, etc.
pub struct Path(pub &'static str);
impl Expr for Path {
    const COMPLEXITY: usize = 1;
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        mutate_todo!()
    }
}

/// An `if` statement without an `else` block.
pub struct If<Cond: Expr, Left: Expr>(pub Cond, pub Left);
impl<Cond: Expr, Left: Expr> Expr for If<Cond, Left> {
    const COMPLEXITY: usize = 1 + Cond::COMPLEXITY + Left::COMPLEXITY;
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        mutate_todo!()
        // eventually: if self.0.mutate() { self.1.mutate() }
    }
}

/// An `if` statement with an `else` block.
pub struct IfElse<Cond: Expr, Left: Expr, Right: Expr>(pub Cond, pub Left, pub Right);
impl<Cond: Expr, Left: Expr, Right: Expr> Expr for IfElse<Cond, Left, Right> {
    const COMPLEXITY: usize = 1 + Cond::COMPLEXITY + Left::COMPLEXITY + Right::COMPLEXITY;
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        mutate_todo!()
        // eventually: if self.0.mutate() { self.1.mutate() } else { self.2.mutate() }
    }
}

/// Addition, e.g. `a + b`.
pub struct Add<Left: Expr, Right: Expr>(pub Left, pub Right);
impl<Left: Expr, Right: Expr> Expr for Add<Left, Right> {
    const COMPLEXITY: usize = 1 + Left::COMPLEXITY + Right::COMPLEXITY;
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        mutate_todo!()
    }
}

/// Subtraction, e.g. `a - b`.
pub struct Sub<Left: Expr, Right: Expr>(pub Left, pub Right);
impl<Left: Expr, Right: Expr> Expr for Sub<Left, Right> {
    const COMPLEXITY: usize = 1 + Left::COMPLEXITY + Right::COMPLEXITY;
    #[inline]
    fn mutate<F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        example_fn: &F,
    ) -> Option<&'static str> {
        mutate_todo!()
    }
}
