extern crate symbol;

#[macro_use]
mod macros;
#[cfg(test)]
mod tests;

use std::{
    collections::{HashMap, HashSet},
    fmt::{Display, Formatter, Result as FmtResult},
    rc::Rc,
};
use symbol::Symbol;

fn elim(inddef: &Inddef) -> Rc<Expr> {
    // NOTE: I'm using "variance" here with a different meaning than is conventional in programming
    // languages; a type argument is _invariant_ if no term directly contains (i.e. contains as an
    // argument to its constructor) an instance of the same base type with a different value for
    // the argument. For example, in the declaration:
    //
    // type Vect : Nat -> TYPE -> TYPE where
    //   nil : {T: TYPE} -> Vect z T
    //   cons : {n: Nat} -> {T: TYPE} -> T -> Vect n T -> Vect (s n) T
    //
    // the Nat argument is variant and the TYPE argument is invariant.

    let &Inddef(name, ref ty_args, ref ctors) = inddef;

    // The arguments to the type. A tuple of (name, type, variant). For variant, true means that
    // the argument is variant, false means that it is invariant.
    let ty_args = ty_args
        .iter()
        .cloned()
        .enumerate()
        .map(|(n, arg_ty)| {
            let arg_name = Symbol::gensym();
            let arg_variant = ctors.iter().any(|ctor| !ctor.is_ind_inv(name, n));
            (arg_name, arg_ty, arg_variant)
        })
        .collect::<Vec<_>>();

    // The type, instantiated with all the args specified in ty_args.
    let ty = Expr::apps(
        var!(ref name),
        ty_args
            .iter()
            .map(|&(arg_name, _, _)| var!(ref arg_name))
            .collect(),
    );

    // The invariant type arguments, which come at the beginning of the type of the eliminator.
    let inv_ty_args = ty_args
        .iter()
        .filter(|(_, _, var)| !*var)
        .map(|&(name, ref ty, _)| (Some(name), ty.clone()))
        .collect::<Vec<_>>();

    // The variant type arguments, which come near the end of the type of the eliminator, before
    // the actual instance of the type does. They also appear in the type of the motive and in each
    // constructor clause.
    let var_ty_args = ty_args
        .iter()
        .filter(|(_, _, var)| *var)
        .map(|&(name, ref ty, _)| (Some(name), ty.clone()))
        .collect::<Vec<_>>();

    // The name and type of the motive.
    let motive = Expr::pis(var_ty_args.clone(), pi![ty.clone(), ty!()]);
    let motive_name = Symbol::gensym();

    // The constructor argument redundancy info.
    let ctor_case_redundancy = ctors
        .iter()
        .map(|&Ctor(_ctor_name, ref ctor_args, ref ctor_ty_args)| {
            ctor_args
                .iter()
                .cloned()
                .enumerate()
                .map(|(n, (name, _))| (n, name.unwrap_or_else(Symbol::gensym)))
                .map(|(n, name)| {
                    let name_expr = var!(ref name);
                    if let Some(ty_arg_idx) = ctor_ty_args.iter().position(|arg| arg == &name_expr)
                    {
                        let ty_arg = &ty_args[ty_arg_idx];
                        if ty_arg.2 {
                            (Symbol::gensym(), false)
                        } else {
                            (ty_arg.0, true)
                        }
                    } else {
                        (Symbol::gensym(), false)
                    }
                })
                .collect::<Vec<_>>()
        })
        .collect::<Vec<_>>();

    let mut ctors = ctors.clone();
    println!("--------------------------------------------------------------------------------");
    for ctor in ctors.iter() {
        println!("{}", ctor);
    }
    println!("================================================================================");
    let mut extra_args = HashMap::new(); // Dumb hack.
    for Ctor(ctor_name, ref mut ctor_args, ref mut ctor_ty_args) in ctors.iter_mut() {
        for n in 0..ctor_ty_args.len() {
            let global_name = ty_args[n].0;
            if !ty_args[n].2 {
                if let Expr::Var(name) = *ctor_ty_args[n] {
                    if let Some(i) = ctor_args.iter().rposition(|&(n, _)| n == Some(name)) {
                        ctor_args.remove(i);
                        extra_args.insert((n, i), global_name);
                        for (_, arg) in ctor_args.iter_mut() {
                            *arg = arg.alpha_one(name, global_name);
                        }
                        for arg in ctor_ty_args.iter_mut() {
                            *arg = arg.alpha_one(name, global_name);
                        }
                    }
                } else {
                    eprintln!("Weirdly shaped invariant arg: {}", ctor_ty_args[n]);
                }
            }
        }
    }
    println!("================================================================================");
    for ctor in ctors.iter() {
        println!("{}", ctor);
    }
    println!("--------------------------------------------------------------------------------");

    // The constructor cases; represent the cases when eliminating the value.
    let ctor_cases = ctors
        .iter()
        .enumerate()
        .map(
            |(ctor_index, &Ctor(ctor_name, ref ctor_args, ref ctor_ty_args))| {
                let mut ctor_args = ctor_args
                    .into_iter()
                    .enumerate()
                    .map(|(n, (name, ty))| (ctor_case_redundancy[ctor_index][n].0, ty.clone()))
                    .collect::<Vec<_>>();

                // The constructor arguments, minus those whose values can be established from the
                // invariant type arguments.
                let non_redundant_ctor_args = ctor_args
                    .iter()
                    .cloned()
                    .enumerate()
                    .filter(|(n, _)| !ctor_case_redundancy[ctor_index][*n].1)
                    .map(|(_, (name, ty))| {
                        let mut ty = ty;

                        (Some(name), ty)
                    })
                    .collect::<Vec<(Option<_>, Rc<_>)>>();

                // The inductive case args; where an inductive term appears, we are able to use the
                // value returned by applying the motive to it.
                let inductive_case_args = ctor_args
                    .iter()
                    .filter(|(_, ty)| {
                        let unapp = ty.unapp();
                        // TODO: Don't be a shithead
                        unapp.len() == ctor_ty_args.len() + 1
                    })
                    .map(|&(n, _)| (None, app![var!(ref motive_name), var!(ref n)]))
                    .collect::<Vec<_>>();

                let mut ctor_args = ctor_args
                    .into_iter()
                    .map(|(n, _)| var!(ref n))
                    .collect::<Vec<_>>();
                extra_args
                    .iter()
                    .filter(|&(&(n, _), _)| n == ctor_index)
                    .for_each(|(&(n, i), ty)| {
                        println!("{}, {}", n, i);
                        ctor_args.insert(i, var!(ref *ty))
                    });

                let mut ctor_case_args = var_ty_args.clone();
                ctor_case_args.extend(ctor_args.iter().map(|ty| (None, ty.clone()))); //non_redundant_ctor_args);
                ctor_case_args.extend(inductive_case_args);
                Expr::pis(
                    ctor_case_args,
                    app![
                        var!(ref motive_name),
                        Expr::apps(var!(ref ctor_name), ctor_args)
                    ],
                )
            },
        )
        .map(|ty| (None, ty))
        .collect::<Vec<_>>();

    let mut all_args = inv_ty_args;
    all_args.push((Some(motive_name), motive));
    all_args.extend(ctor_cases);
    all_args.extend(var_ty_args);
    let ty_val_name = Symbol::gensym();
    all_args.push((Some(ty_val_name), ty));
    Expr::pis(
        all_args,
        Rc::new(Expr::App(
            Rc::new(Expr::Var(motive_name)),
            Rc::new(Expr::Var(ty_val_name)),
        )),
    )
}

#[derive(Clone, Debug)]
struct Inddef(Symbol, Vec<Rc<Expr>>, Vec<Ctor>);

#[derive(Clone, Debug)]
struct Ctor(Symbol, Vec<(Option<Symbol>, Rc<Expr>)>, Vec<Rc<Expr>>);

impl Ctor {
    /// Returns the indices that are an inductive argument.
    fn ind_idxes(&self, ty_name: Symbol) -> Vec<usize> {
        let mut idxs = Vec::new();
        for (i, (n, e)) in self.1.iter().enumerate() {
            if e.appfn() == &Expr::Var(ty_name) {
                idxs.push(i);
            }
            if n == &Some(ty_name) {
                break;
            }
        }
        idxs
    }

    /// Returns whether the numbered argument to the type constructor is "inductively invariant,"
    /// i.e. that within this constructor, the arguments do not directly contain a term of the same
    /// type constructor with different arguments.
    ///
    /// TODO: "Different arguments" ought to be "arguments with different normal forms," but I
    /// don't define normalization here.
    /// TODO: There's probably a better term for "inductively invariant."
    pub fn is_ind_inv(&self, ty_name: Symbol, n: usize) -> bool {
        let ty_arg: &Expr = &self.2[n];
        self.ind_idxes(ty_name).into_iter().all(|i| {
            let arg = &self.1[i];
            let ind_ty_arg: &Expr = arg.1.unapp()[n + 1];
            ty_arg == ind_ty_arg
        })
    }
}

impl Display for Ctor {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        write!(
            fmt,
            "{} : {}",
            self.0,
            Expr::pis(self.1.clone(), Expr::apps(var!(____), self.2.clone()))
        )
    }
}

#[derive(Debug)]
enum Expr {
    App(Rc<Expr>, Rc<Expr>),
    Pi(Symbol, Rc<Expr>, Rc<Expr>),
    Ty,
    Var(Symbol),
}

impl Expr {
    fn alpha(&self) -> Rc<Expr> {
        Rc::new(match *self {
            Expr::App(ref l, ref r) => Expr::App(l.alpha(), r.alpha()),
            Expr::Pi(n, ref l, ref r) => {
                let to = Symbol::gensym();
                Expr::Pi(n, l.alpha_one(n, to), r.alpha_one(n, to))
            }
            Expr::Ty => Expr::Ty,
            Expr::Var(n) => Expr::Var(n),
        })
    }

    fn alpha_one(&self, from: Symbol, to: Symbol) -> Rc<Expr> {
        Rc::new(match *self {
            Expr::App(ref l, ref r) => Expr::App(l.alpha_one(from, to), r.alpha_one(from, to)),
            Expr::Pi(n, ref l, ref r) if n == from => Expr::Pi(n, l.clone(), r.clone()),
            Expr::Pi(n, ref l, ref r) => Expr::Pi(n, l.alpha_one(from, to), r.alpha_one(from, to)),
            Expr::Ty => Expr::Ty,
            Expr::Var(n) if n == from => Expr::Var(to),
            Expr::Var(n) => Expr::Var(n),
        })
    }

    fn appfn(&self) -> &Expr {
        match self {
            &Expr::App(ref l, _) => l.appfn(),
            e => e,
        }
    }

    fn apps(mut func: Rc<Expr>, args: Vec<Rc<Expr>>) -> Rc<Expr> {
        for arg in args {
            func = Rc::new(Expr::App(func, arg));
        }
        func
    }

    fn freevars(&self) -> HashSet<Symbol> {
        match *self {
            Expr::App(ref l, ref r) => {
                let mut fv = l.freevars();
                fv.extend(r.freevars());
                fv
            }
            Expr::Pi(n, ref l, ref r) => {
                let mut rv = r.freevars();
                rv.remove(&n);
                let mut lv = l.freevars();
                lv.extend(rv);
                lv
            }
            Expr::Ty => HashSet::new(),
            Expr::Var(n) => {
                let mut fv = HashSet::new();
                fv.insert(n);
                fv
            }
        }
    }

    fn pis(mut v: Vec<(Option<Symbol>, Rc<Expr>)>, r: Rc<Expr>) -> Rc<Expr> {
        let mut ty = r;
        while let Some((name, arg)) = v.pop() {
            ty = Rc::new(Expr::Pi(name.unwrap_or_else(Symbol::gensym), arg, ty));
        }
        ty
    }

    fn unapp(&self) -> Vec<&Expr> {
        match self {
            &Expr::App(ref l, ref r) => {
                let mut v = l.unapp();
                v.push(r);
                v
            }
            e => vec![e],
        }
    }
}

impl Display for Expr {
    fn fmt(&self, fmt: &mut Formatter) -> FmtResult {
        match *self {
            Expr::App(ref l, ref r) => write!(fmt, "{} ({})", l, r),
            Expr::Pi(n, ref l, ref r) => {
                if r.freevars().contains(&n) {
                    write!(fmt, "({} : {}) -> {}", n, l, r)
                } else {
                    write!(fmt, "({}) -> {}", l, r)
                }
            }
            Expr::Ty => write!(fmt, "*"),
            Expr::Var(n) => write!(fmt, "{}", n),
        }
    }
}

impl PartialEq for Expr {
    fn eq(&self, other: &Expr) -> bool {
        match (self, other) {
            (&Expr::App(ref ll, ref lr), &Expr::App(ref rl, ref rr)) => ll == rl && lr == rr,
            (&Expr::Pi(ln, ref ll, ref lr), &Expr::Pi(rn, ref rl, ref rr)) => {
                let sym = Symbol::gensym();
                let l = lr.alpha_one(ln, sym);
                let r = rr.alpha_one(rn, sym);
                ll == rl && l == r
            }
            (&Expr::Ty, &Expr::Ty) => true,
            (&Expr::Var(ln), &Expr::Var(rn)) => ln == rn,
            _ => false,
        }
    }
}
