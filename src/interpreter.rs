use core::any::Any;
use hashbrown::HashMap;
use syn::{Stmt::*, __private::ToTokens};

pub struct State {
    idents: HashMap<String, Box<dyn Any>>,
}

impl State {
    pub fn new() -> Self {
        Self {
            idents: HashMap::new(),
        }
    }
    pub fn insert<T: 'static>(&mut self, ident: String, value: T) {
        if self.idents.insert(ident, Box::new(value)).is_some() {
            panic!()
        }
    }
    pub fn get(&self, ident: &str) -> &Box<dyn Any> {
        self.idents
            .get(ident)
            .expect("Requested identifier not in scope")
    }
}

pub trait Interpret {
    fn run(&self, ast: &Vec<syn::Stmt>) -> &Box<dyn Any>;
    fn eval(&self, e: &syn::Expr) -> &Box<dyn Any>;
}

impl Interpret for State {
    fn run(&self, ast: &Vec<syn::Stmt>) -> &Box<dyn Any> {
        let Some((head, tail)) = ast.split_first() else { panic!("Function ended without returning a value"); };
        dbg!(head);
        match head {
            Expr(e, None) => {
                assert!(tail.is_empty());
                self.eval(e)
            }
            _ => {
                dbg!(head);
                todo!()
            }
        }
    }

    fn eval(&self, e: &syn::Expr) -> &Box<dyn Any> {
        match e {
            syn::Expr::Path(p) => self.get(&p.path.to_token_stream().to_string()),
            // syn::Expr::Macro(m) => ,
            _ => {
                dbg!(e);
                todo!()
            }
        }
    }
}
