use std::collections::HashMap;
use std::collections::hash_map::DefaultHasher;
use std::hash::Hash;
use std::hash::Hasher;
use std::rc::Rc;
use stable_hash;

use crate::lambda_term::LambdaTerm;
use crate::lambda_term::LambdaTermR;
use crate::lambda_term::LambdaErr;
use crate::lambda_term::LambdaErr::{BadApp, Stuck, Unbound};
use crate::lambda_term::{appr, absr, varr, primr};
use crate::lambda_term::LambdaTerm::{App, Abs, Var, Primitive};
use crate::lambda_term::Prim;

pub type Binder = u128;
pub type Lbinder = u128;

type LambdaTm = crate::lambda_term::LambdaTerm<Binder, Lbinder, Prim>;
type LambdaTmR = crate::lambda_term::LambdaTermR<Binder, Lbinder, Prim>;

type LambdaTmDb = crate::lambda_term::LambdaTerm<crate::de_bruijn::Binder, crate::de_bruijn::Lbinder, Prim>;
type LambdaTmDbR = crate::lambda_term::LambdaTermR<crate::de_bruijn::Binder, crate::de_bruijn::Lbinder, Prim>;

// TODO: change this to use stable hash to save on hash computations
struct Sub {
    sb : HashMap<Binder, LambdaTmR>
}

type SubR = Rc<Sub>;

// first, expand width to u128
fn expand_binder_rep(t : LambdaTmDbR) -> LambdaTmR {
    match &*t {
        App(body, arg) =>
            appr(expand_binder_rep(body.clone()), expand_binder_rep(arg.clone())),
        Abs(n, body) =>
            absr(0u128, expand_binder_rep(body.clone())),
        Var(v) => varr(*v as u128 ),
        Primitive(p) => primr(p.clone()),
    }
}

// constructing nameful.
// do we need to hash before or after converting from de bruijn?
// - get hash

// after calculating a hash, we need to substitute it into the body
fn nameful_rename1_db(t : LambdaTmR, n : u128, depth : u64) -> LambdaTmR {
    match &*t {
        App(body, arg) => {
            appr(nameful_rename1_db(body.clone(), n, depth), nameful_rename1_db(arg.clone(), n, depth))
        },
        Abs(bind, body) => {
            absr(*bind, nameful_rename1_db(body.clone(), n, depth + 1))
        },
        Var(v) => {
            if *v == depth as u128 {
                varr(n)
            } else {
                t
            }
        },
        Primitive(_) => t,
    }
}

// TODO: see if we can reduce the need for repeated computation of hashes
fn construct_nameful(t : LambdaTmR) -> LambdaTmR {
    match &*t {
        App(body, arg) => {
            appr(construct_nameful(body.clone()),construct_nameful(arg.clone()))
        },

        Abs(n, body) => {
            let mut hasher = DefaultHasher::new();
            body.clone().hash(&mut hasher);
            let hash = hasher.finish() as u128;
            let body_new = nameful_rename1_db(body.clone(), hash, 0);
            let body_new = construct_nameful(body_new);
            absr(hash, body_new)
        },

        Var(_) => t,
        Primitive(p) => primr(p.clone()),
    }
}

// apply substitution to a term
// this probably doesn't work in general for substituting under binders
fn subst(s : &Sub, t : LambdaTmR) -> LambdaTmR {
    match &*t {
        App(t1, t2) => {
            appr(subst(s, t1.clone()), subst(s, t2.clone()))
        },
        Abs(n, t) => {
            absr(*n, subst(s, t.clone()))
        },
        Var(i) => {
            apply(s, *i)
        },
        Primitive(_) => t
    }
}

// get substitution's value for a particular variable
fn apply(s : &Sub, x : Binder) -> LambdaTmR {
    match s.sb.get(&x) {
        Some(t) => t.clone(),
        None => varr(x), // TODO: should this be an error?
    }
}

fn single_sub(n : u128, t : LambdaTmR) -> SubR {
    let mut hm = HashMap::new();
    hm.insert(n, t);
    Rc::new(Sub { sb : hm })
}

fn beta_step_app(body : LambdaTmR, arg : LambdaTmR) -> Result<LambdaTmR, LambdaErr> {
    match &*body {
        App(body2, arg2) => {
            let new_body = beta_step_app(body2.clone(), arg2.clone())?;
            Ok(appr(new_body, arg))
        },
        Abs(n, body2) => {
            Ok(subst(&single_sub(*n, arg), body2.clone()))
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

#[test]
fn subst1() {
    // (\ x y . y x) ()
    let term1 = absr ((), absr ((), appr (varr(0), varr(1))));
    let term2 = varr(999);
    let term3 = appr(term1.clone(), term2.clone());

    let the_term = construct_nameful(expand_binder_rep(term3));

    println!("Nameful term: {:?}", the_term);

    let result = beta_step(the_term);
    println!("Nameful Result: {:?}", result);
}

/*
#[test]
fn subst2() {
    // (\x . x x) (\x . x x)
    let f = absr((), appr(varr(0), varr(0)));
    let omega = appr(f.clone(), f.clone());
    let result = beta_steps(10, omega);
    println!("Omega: {:?}", result);
}
*/