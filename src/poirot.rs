#![allow(dead_code)]

/// Checks that the result of a unary function call is equal to its argument.
pub fn does_nothing<T: PartialEq, F: Fn(&T) -> &T>(f: F, x: &T) -> bool {
    f(x) == x
}

/// Checks that the result of a unary function call is NOT equal to its argument.
pub fn does_something<T: PartialEq, F: Fn(&T) -> &T>(f: F, x: &T) -> bool {
    f(x) != x
}

#[test]
fn id_automatic_test() {
    let _: Option<()> = crate::id_check_cover(&crate::id)
        .and_then(|e| panic!("{}", e.replace("crate::poirot::", "")));
}
