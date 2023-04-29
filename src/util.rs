//! Defines the `timid_assert` macro to fail with a message without `panic`king.

/// Like an ordinary assertion, except success yields `None` and error yields the error message.
#[macro_export]
macro_rules! timid_assert {
    ($cond:expr) => {
        if (!$cond) {
            Some(stringify!($cond))
        } else {
            None
        }
    };
}

pub use timid_assert;
