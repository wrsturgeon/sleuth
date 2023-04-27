//! Extremely opinionated testing framework producing an exact specification alongside an almost provably optimal implementation.

#![deny(warnings, missing_docs)]

// mod ast;
// mod interpreter;

pub use brute_mutator::*;

fn main() -> anyhow::Result<()> {
    // let functions = ast::functions_in_file(MAIN_FILE)?;
    // let tests = ast::functions_in_file(TEST_FILE)?;
    // dbg!(&functions);

    // for f in functions {
    //     for t in &tests {
    //         let mut state = interpreter::State::new();
    //         for input in &f.sig.inputs {
    //             let syn::FnArg::Typed(arg) = input else { panic!() };
    //             let name = match *arg.pat.clone() {
    //                 syn::Pat::Ident(name) => name.ident.to_string(),
    //                 _ => todo!(),
    //             };
    //             state.insert(name, 0);
    //         }

    //         state
    //             .run(&t.block.stmts)
    //             .downcast_ref::<()>()
    //             .ok_or(anyhow!("Tests shouldn't return values (other than `()`)"))?;
    //     }

    //     println!();
    //     println!("{}", ast::as_source_code(f));
    // }

    assert!(does_nothing(id, &0));

    Ok(())
}

#[cover(does_nothing(0), does_nothing(1))]
fn id<A>(a: A) -> A {
    a
}

// #[fn_predicate] // ==> fn does_nothing<T, F, R: fn(F) -> bool>(x: &T) -> R { move |f| f(x) == x }
fn does_nothing<T: PartialEq, F: FnOnce(&T) -> &T>(f: F, x: &T) -> bool {
    f(x) == x
}
