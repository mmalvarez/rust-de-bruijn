use std::{rc::Rc, borrow::Borrow};
use either::Either;
use LambdaErr::BadApp;

pub type Binder = u64;

#[derive(Clone, Debug)]
pub enum Prim {
    Unit(()),
    Int(u64),
    String(String),
    Bool(bool),
    Pair(Rc<Prim>, Rc<Prim>),
    Fn(fn(Prim) -> Prim),
}

#[derive(Clone, Debug)]
pub enum LambdaTerm {
    App(Rc<LambdaTerm>, Rc<LambdaTerm>),
    Abs(Rc<LambdaTerm>),
    Var(Binder),
    Primitive(Prim),
}

impl LambdaTerm {

    pub fn appr(body : LambdaTermR, arg : LambdaTermR) -> LambdaTermR {
        Rc::new(App(body, arg))
    }
    pub fn absr(body : LambdaTermR) -> LambdaTermR {
        Rc::new(Abs(body))
    }
    pub fn varr(x : Binder) -> LambdaTermR {
        Rc::new(Var(x))
    }
    pub fn primr(p : Prim) -> LambdaTermR {
        Rc::new(Primitive(p))
    }
}

pub fn appr(body : LambdaTermR, arg : LambdaTermR) -> LambdaTermR {
    Rc::new(App(body, arg))
}
pub fn absr(body : LambdaTermR) -> LambdaTermR {
    Rc::new(Abs(body))
}
pub fn varr(x : Binder) -> LambdaTermR {
    Rc::new(Var(x))
}
pub fn primr(p : Prim) -> LambdaTermR {
    Rc::new(Primitive(p))
}


pub type LambdaTermR = Rc<LambdaTerm>;

use LambdaTerm::{App, Abs, Var, Primitive};

#[derive(Debug)]
pub enum LambdaErr {
    // Applying something that's not a function
    BadApp
}

// increment or decrement free-variable indices in a lambda term
fn shift1(body : LambdaTermR, decr : bool, depth : Binder) -> LambdaTermR {
    match &*body {
        App(body2, arg2) => {
            Rc::new(App(shift1(body2.clone(), decr, depth), shift1(arg2.clone(), decr, depth)))
        },
        Abs(body2) => {
            Rc::new(Abs(shift1(body2.clone(), decr, depth + 1)))
        },
        Var(v) => {
            if *v >= depth {
                let vnew = if decr { v - 1 } else { v + 1 };
                Rc::new(Var(vnew))
            } else {
                body
            }
        },
        Primitive(_) => body,
    }
}

// Substitute a lambda term into another lambda term, at the given index
// TODO: do we need parallel substitution?
fn subst_at(body : LambdaTermR, arg : LambdaTermR, index : Binder) -> LambdaTermR {
    match &*body {
        App(body2, arg2) => {
            Rc::new(App(subst_at(body2.clone(), arg.clone(), index), subst_at(arg2.clone(), arg.clone(), index)))
        },
        Abs(body2) => {
            // TODO: depth?
            let body2 = shift1(body2.clone(), true, 1);
            println!("Subst under Abs. Body2: {:?}", body2);
            // TODO: depth?
            let arg = shift1(arg, false, 1);
            println!("Subst under abs. Arg : {:?}", arg);
            Rc::new(Abs(subst_at(body2, arg, index + 1)))
        },
        Var(v) => {
            if *v == index {
                arg
            } else {
                body
            }
        },
        Primitive(_) => body
    }
}

fn beta_step(body : LambdaTermR) -> Result<LambdaTermR, LambdaErr> {
    match &*body {
        App(body2, arg2) => {
            match &**body2 {
                App(_, _) => {
                    let result = beta_step(body2.clone())?;
                    Ok(Rc::new(App(result, arg2.clone())))
                },
                Abs(body3) => {
                    let result = subst_at(body3.clone(), arg2.clone(), 1);
                    Ok(result)
                },
                Var(_) => {
                    Err(BadApp)
                },
                Primitive(_) => {
                    // TODO: handle the case of applying a primitive function
                    Err(BadApp)
                }
            }
        },
        Abs(body2) => {
            let result = beta_step(body2.clone())?;
            Ok(Rc::new(Abs(result)))
        },
        Var(v) => {
            Err(BadApp)
        },
        Primitive(p) => {
            Err(BadApp)
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::LambdaTerm::{App, Abs, Var, Primitive};
    //use crate::LambdaTerm::{appr, absr, varr, primr};
    use crate::{appr, absr, varr, primr};
    use crate::beta_step;


    #[test]
    fn subst1() {
        // (\ x y . y x) ()
        let term1 = absr (absr (appr (varr(1), varr(2))));
        let term2 = varr(999);
        let result = beta_step(appr(term1.clone(), term2.clone()));
        println!("Result: {:?}", result);
    }
}
