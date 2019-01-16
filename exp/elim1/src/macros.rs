macro_rules! app {
    ($e:expr) => { $e };
    ($f:expr, $x:expr $(,$xs:expr)*) => { app!(Rc::new(Expr::App($f, $x)) $(,$xs)*) };
}

macro_rules! pi {
    ($e:expr $(,)*) => { $e };
    ($n:ident : $h:expr, $($t:tt)*) => { Rc::new(Expr::Pi(s!($n), $h, pi!($($t)*))) };
    ($h:expr, $($t:tt)*) => { Rc::new(Expr::Pi(Symbol::gensym(), $h, pi!($($t)*))) };
}

macro_rules! s {
    ($i:ident) => {
        Symbol::from(stringify!($i))
    };
}

macro_rules! ty {
    () => {
        Rc::new(Expr::Ty)
    };
}

macro_rules! var {
    ($i:ident) => {
        var!(ref s!($i))
    };
    (ref $e:expr) => {
        Rc::new(Expr::Var($e))
    };
}
