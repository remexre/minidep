use crate::{elim, Ctor, Expr, Inddef};
use std::rc::Rc;
use symbol::Symbol;

fn test(inddef: Inddef, correct_elim: Rc<Expr>) {
    let elim = elim(&inddef);
    println!("        elim = {}", elim);
    println!("correct_elim = {}", correct_elim);
    assert_eq!(elim, correct_elim);
}

#[test]
fn void() {
    test(
        Inddef(s!(Void), vec![], vec![]),
        pi![
            m: pi![var!(Void), ty!()],
            x: var!(Void),
            app!(var!(m), var!(x)),
        ],
    );
}

#[test]
fn unit() {
    test(
        Inddef(s!(Unit), vec![], vec![Ctor(s!(unit), vec![], vec![])]),
        pi![
            m: pi![var!(Unit), ty!()],
            app!(var!(m), var!(unit)),
            x: var!(Unit),
            app!(var!(m), var!(x)),
        ],
    );
}

#[test]
fn bool() {
    test(
        Inddef(
            s!(Bool),
            vec![],
            vec![
                Ctor(s!(true), vec![], vec![]),
                Ctor(s!(false), vec![], vec![]),
            ],
        ),
        pi![
            m: pi![var!(Bool), ty!()],
            app!(var!(m), var!(true)),
            app!(var!(m), var!(false)),
            b: var!(Bool),
            app!(var!(m), var!(b)),
        ],
    );
}

#[test]
fn nat() {
    test(
        Inddef(
            s!(Nat),
            vec![],
            vec![
                Ctor(s!(z), vec![], vec![]),
                Ctor(s!(s), vec![(None, var!(Nat))], vec![]),
            ],
        ),
        pi![
            m: pi![var!(Nat), ty!()],
            app!(var!(m), var!(z)),
            pi![
                n: var!(Nat),
                app!(var!(m), var!(n)),
                app!(var!(m), app!(var!(s), var!(n)))
            ],
            n: var!(Nat),
            app!(var!(m), var!(n)),
        ],
    );
}

#[test]
fn list() {
    test(
        Inddef(
            s!(List),
            vec![ty!()],
            vec![
                Ctor(s!(nil), vec![(Some(s!(T)), ty!())], vec![var!(T)]),
                Ctor(
                    s!(cons),
                    vec![
                        (Some(s!(T)), ty!()),
                        (None, var!(T)),
                        (None, app!(var!(List), var!(T))),
                    ],
                    vec![var!(T)],
                ),
            ],
        ),
        pi![
            T: ty!(),
            m: pi![app!(var!(List), var!(T)), ty!()],
            app!(var!(m), app!(var!(nil), var!(T))),
            pi![
                h: var!(T),
                t: app!(var!(List), var!(T)),
                app!(var!(m), var!(t)),
                app!(var!(m), app!(var!(cons), var!(T), var!(h), var!(t)))
            ],
            l: app!(var!(List), var!(T)),
            app!(var!(m), var!(l)),
        ],
    );
}

#[test]
fn vect() {
    test(
        Inddef(
            s!(Vect),
            vec![ty!(), var!(Nat)],
            vec![
                Ctor(s!(vnil), vec![(Some(s!(T)), ty!())], vec![var!(T), var!(z)]),
                Ctor(
                    s!(vcons),
                    vec![
                        (Some(s!(T)), ty!()),
                        (Some(s!(n)), var!(Nat)),
                        (None, var!(T)),
                        (None, app!(var!(Vect), var!(T), var!(n))),
                    ],
                    vec![var!(T), app!(var!(s), var!(n))],
                ),
            ],
        ),
        pi![
            T: ty!(),
            m: pi![n: var!(Nat), app!(var!(Vect), var!(T), var!(n)), ty!()],
            pi![n: var!(Nat), app!(var!(m), app!(var!(vnil), var!(T)))],
            pi![
                n: var!(Nat),
                h: var!(T),
                t: app!(var!(Vect), var!(T), var!(n)),
                app!(var!(m), var!(t)),
                app!(
                    var!(m),
                    app!(
                        var!(vcons),
                        var!(T),
                        app!(var!(s), var!(n)),
                        var!(h),
                        var!(t)
                    )
                )
            ],
            n: var!(Nat),
            v: app!(var!(Vect), var!(T), var!(n)),
            app!(var!(m), var!(v)),
        ],
    );
}
