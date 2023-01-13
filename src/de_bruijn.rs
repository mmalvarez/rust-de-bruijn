use std::rc::Rc;
use crate::lambda_term::LambdaTerm;
use crate::lambda_term::LambdaTermR;
use crate::lambda_term::LambdaErr;
use crate::lambda_term::LambdaErr::{BadApp, Stuck, Unbound};
use crate::lambda_term::{appr, absr, varr, primr};
use crate::lambda_term::LambdaTerm::{App, Abs, Var, Primitive};
use crate::lambda_term::Prim;

pub type Binder = u64;
pub type Lbinder = ();

#[derive(Clone)]
pub enum Sub {
    Incr(Binder),
    Cons(crate::lambda_term::LambdaTermR<Binder, Lbinder, Prim>, Rc<Sub>),
    Comp(Rc<Sub>, Rc<Sub>),
}

type LambdaTm = crate::lambda_term::LambdaTerm<Binder, Lbinder, Prim>;
type LambdaTmR = crate::lambda_term::LambdaTermR<Binder, Lbinder, Prim>;

use Sub::{Incr, Cons, Comp};
type SubR = Rc<Sub>;

pub fn incrr(x : Binder) -> SubR {
    Rc::new(Incr(x))
}
pub fn consr(t : LambdaTmR, s : SubR) -> SubR {
    Rc::new(Cons(t, s))
}
pub fn compr(s1 : SubR, s2 : SubR) -> SubR {
    Rc::new(Comp(s1, s2))
}

fn lift(s : SubR) -> SubR {
    consr(varr(0), compr(s, incrr(1)))
}

fn nil_sub() -> SubR {
    incrr(0)
}

// bind 0 to the given term
fn single_sub(t : LambdaTmR) -> SubR {
    consr(t, nil_sub())
}

// apply substitution to a term
fn subst(s : SubR, t : LambdaTmR) -> LambdaTmR {
    match &*t {
        App(t1, t2) => {
            appr(subst(s.clone(), t1.clone()), subst(s.clone(), t2.clone()))
        },
        Abs(n, t) => {
            absr((), subst(lift(s), t.clone()))
        },
        Var(i) => {
            apply(s, *i)
        },
        Primitive(_) => t
    }
}

// get substitution's value for a particular variable
fn apply(s : SubR, x : Binder) -> LambdaTmR {
    match &*s {
        Sub::Incr(k) => varr(k + x),
        Sub::Cons(t, s) => {
            if x == 0 {
                t.clone()
            } else {
                apply(s.clone(), x - 1)
            }
        },
        Sub::Comp(s1, s2) => {
            let after_s1 = apply(s1.clone(), x);
            subst(s2.clone(), after_s1)
        },
    }
}

fn beta_step_app(body : LambdaTmR, arg : LambdaTmR) -> Result<LambdaTmR, LambdaErr> {
    match &*body {
        App(body2, arg2) => {
            let new_body = beta_step_app(body2.clone(), arg2.clone())?;
            Ok(appr(new_body, arg))
        },
        Abs(n, body2) => {
            Ok(subst(single_sub(arg), body2.clone()))
        },
        Var(_) => Err(Unbound),
        Primitive(_) => Err(BadApp),
    }
}

fn beta_step(body : LambdaTmR) -> Result<LambdaTmR, LambdaErr> {
    match &*body {
        App(body, arg) => {
            beta_step_app(body.clone(), arg.clone())
        },
        Abs(_,_) => Err(Stuck),
        Var(_) => Err(Unbound),
        Primitive(_) => Err(Stuck),
    }
}

fn beta_steps(steps : u64, t : LambdaTmR) -> Result<LambdaTmR, LambdaErr> {
    let mut t = t;
    for i in [0..steps] {
        t = beta_step(t)?;
    }
    Ok(t)
}

fn beta_eval(t : LambdaTmR) -> Result<LambdaTmR, LambdaErr> {
    let mut t = t;
    loop {
        match beta_step(t.clone()) {
            Ok(t2) => {
                t = t2;
            },
            Err(e) => {
                return match e {
                    BadApp => Err(BadApp),
                    Unbound => Err(Unbound),
                    Stuck => Ok(t),
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::lambda_term::LambdaTerm::{App, Abs, Var, Primitive};
    use crate::lambda_term::{appr, absr, varr, primr};
    use crate::de_bruijn::{beta_step, beta_steps};


    #[test]
    fn subst1() {
        // (\ x y . y x) ()
        let term1 = absr ((), absr ((), appr (varr(0), varr(1))));
        let term2 = varr(999);
        let result = beta_step(appr(term1.clone(), term2.clone()));
        println!("Result: {:?}", result);
    }

    #[test]
    fn subst2() {
        // (\x . x x) (\x . x x)
        let f = absr((), appr(varr(0), varr(0)));
        let omega = appr(f.clone(), f.clone());
        let result = beta_steps(10, omega);
        println!("Omega: {:?}", result);
    }
}
