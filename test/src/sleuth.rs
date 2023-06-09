//! Module containing properties for use in `sleuth` attributes.

#![allow(dead_code)]

/// Checks that the result of a unary function call is equal to its argument.
pub fn does_nothing<T: PartialEq + Clone, F: Fn(T) -> T>(f: F, x: T) -> bool {
    x.clone() == f(x)
}

/// Checks that the result of a unary function call is NOT equal to its argument.
pub fn does_something<T: PartialEq + Clone, F: Fn(T) -> T>(f: F, x: T) -> bool {
    x.clone() != f(x)
}

/// For two functions `f` & `g`, checks that `f(g(x)) == x`.
pub fn roundtrip<T, U, F, G>(f: F, g: G, x: T) -> bool
where
    T: PartialEq + Clone,
    F: Fn(U) -> T,
    G: Fn(T) -> U,
{
    x.clone() == f(g(x))
}
