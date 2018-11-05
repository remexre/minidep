grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

synthesized attribute eq :: (Boolean ::= Expr) occurs on Expr;

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.eq = \e::Expr -> case e of
  | app(f2, x2) -> f.eq(f2) && x.eq(x2)
  | _ -> false
  end;
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  top.eq = \e::Expr -> case e of
  | lam(name2, body2) -> name == name2 && body.eq(body2)
  | _ -> false
  end;
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.eq = \e::Expr -> case name, e of
  | just(n), pi(just(n2), l2, r2) -> n == n2 && l.eq(l2) && r.eq(r2)
  | nothing(), pi(nothing(), l2, r2) -> l.eq(l2) && r.eq(r2)
  | _, _ -> false
  end;
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.eq = \e::Expr -> case e of
  | unificationVar(id2) -> id == id2
  | _ -> false
  end;
}

aspect production var
top::Expr ::= s::String
{
  top.eq = \e::Expr -> case e of
  | var(s2) -> s == s2
  | _ -> false
  end;
}
