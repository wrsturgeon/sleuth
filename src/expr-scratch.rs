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
    fn eval<Scope, F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        scope: &mut Scope,
    ) -> Result<Return, &'static str>;
}

/// Wrapper around the last statement in a block without a semicolon, locally returning its value.
pub struct LastStatement<Return, S: Expr<Return>>(pub S);
impl<Return, S: Expr<Return>> Expr<Return> for LastStatement<Return, S> {
    const COMPLEXITY: usize = S::COMPLEXITY;
    #[inline]
    fn eval<Scope, F, C: Fn(&F) -> Option<&'static str>>(
        &self,
        check: C,
        scope: &mut Scope,
        example_fn: &F,
    ) -> Result<Scope, &'static str> {
        self.0
            .eval(&check, scope, example_fn)
            .or_else(|| self.1.eval(&check, scope, example_fn))
    }
}
impl<Return, S: Expr<Return>> StatementNode<Return> for LastStatement<Return, S> {}
