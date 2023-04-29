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
