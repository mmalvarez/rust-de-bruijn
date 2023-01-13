use std::rc::Rc;
use stable_hash::StableHash;

#[derive(Debug, Hash, Clone)]
pub enum Prim {
    Unit(()),
    Int(u64),
    String(String),
    Bool(bool),
    Pair(Rc<Prim>, Rc<Prim>),
    Fn(fn(Prim) -> Prim),
}

#[derive(Clone, Debug, Hash)]
pub enum LambdaTerm<B, L, P> {
    App(Rc<LambdaTerm<B, L, P>>, Rc<LambdaTerm<B, L, P>>),
    Abs(L, Rc<LambdaTerm<B, L, P>>),
    Var(B),
    Primitive(P),
}
use LambdaTerm::{App, Abs, Var, Primitive};

impl<B : StableHash, L : StableHash, P : StableHash> StableHash for LambdaTerm<B, L, P> {
    fn stable_hash<H: stable_hash::StableHasher>(&self, field_address: H::Addr, state: &mut H) {
        todo!()
    }
}

pub type LambdaTermR<B, L, P> = Rc<LambdaTerm<B, L, P>>;

pub fn appr<B, L, P>(body : LambdaTermR<B, L, P>, arg : LambdaTermR<B, L, P>) -> LambdaTermR<B, L, P> {
    Rc::new(App(body, arg))
}
pub fn absr<B, L, P>(bind : L, body : LambdaTermR<B, L, P>) -> LambdaTermR<B, L, P> {
    Rc::new(Abs(bind, body))
}
pub fn varr<B, L, P>(x : B) -> LambdaTermR<B, L, P> {
    Rc::new(Var(x))
}
pub fn primr<B, L, P>(p : P) -> LambdaTermR<B, L, P> {
    Rc::new(Primitive(p))
}

#[derive(Debug)]
pub enum LambdaErr {
    // Applying something that's not a function
    BadApp,
    // Unbound varible
    Unbound,
    // Stuck Term
    Stuck,
}