//! Defines the `timid_assert` macro to fail with a message without `panic`king.

/// Like an ordinary assertion, except success yields `None` and error yields the error message.
#[macro_export]
macro_rules! timid_assert {
    ($cond:expr) => {
        match std::panic::catch_unwind(|| $cond) {
            Ok(b) => {
                if !b {
                    Some(stringify!($cond))
                } else {
                    None
                }
            }
            _ => Some(concat!(
                stringify!($cond),
                " PANICKED (see two lines above)"
            )),
        }
    };
}

/// Like an ordinary assertion, except success yields `None` and error yields the error message.
#[macro_export]
macro_rules! timid_assert_false {
    ($cond:expr) => {
        match std::panic::catch_unwind(|| $cond) {
            Ok(b) => {
                if b {
                    Some(concat!("!", stringify!($cond)))
                } else {
                    None
                }
            }
            _ => Some(concat!(
                "!",
                stringify!($cond),
                " PANICKED (see two lines above)"
            )),
        }
    };
}

pub use {timid_assert, timid_assert_false};
