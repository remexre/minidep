grammar edu:umn:cs:melt:minidep:abstractsyntax:spined;

synthesized attribute eqExprs :: (Boolean ::= Exprs) occurs on Exprs;

aspect production exprsCons
top::Exprs ::= hd::Expr tl::Exprs
{
  top.eqExprs = \es::Exprs -> case es of
  | exprsCons(hd2, tl2) -> hd.eqExpr(hd2) && tl.eqExprs(tl2)
  | exprsNil() -> false
  end;
}

aspect production exprsNil
top::Exprs ::=
{
  top.eqExprs = \es::Exprs -> case es of
  | exprsCons(_, _) -> false
  | exprsNil() -> true
  end;
}

synthesized attribute eqExpr :: (Boolean ::= Expr) occurs on Expr;

aspect production call
top::Expr ::= f::String xs::Exprs
{
  top.eqExpr = \e::Expr -> case e of
  | call(f2, xs2) -> f == f2 && xs.eqExprs(xs2)
  | _ -> false
  end;
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  top.eqExpr = \e::Expr -> case e of
  | lam(name2, body2) -> name == name2 && body.eqExpr(body2)
  | _ -> false
  end;
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.eqExpr = \e::Expr -> case name, e of
  | just(n), pi(just(n2), l2, r2) -> n == n2 && l.eqExpr(l2) && r.eqExpr(r2)
  | nothing(), pi(nothing(), l2, r2) -> l.eqExpr(l2) && r.eqExpr(r2)
  | _, _ -> false
  end;
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.eqExpr = \e::Expr -> case e of
  | unificationVar(id2) -> id == id2
  | _ -> false
  end;
}
