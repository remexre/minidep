grammar edu:umn:cs:melt:minidep:abstractsyntax:implicit;

import edu:umn:cs:melt:minidep:concretesyntax;
import silver:langutil;
import silver:langutil:pp;

function ppExpr
Document ::= ex::Expr
{
  return ex.expr1.pp;
}

function ppsExpr
String ::= ex::Expr
{
  return show(80, ppExpr(ex));
}

synthesized attribute expr1 :: Expr1_c occurs on Expr;
synthesized attribute expr2 :: Expr2_c occurs on Expr;
synthesized attribute expr3 :: Expr3_c occurs on Expr;
synthesized attribute expr4 :: Expr4_c occurs on Expr;
synthesized attribute expr5 :: Expr5_c occurs on Expr;

aspect production lam
top::Expr ::= arg::String body::Expr
{
  top.expr1 = lam_c('\', terminal(Name_t, arg, top.location), '.', body.expr1,
    location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production lamI
top::Expr ::= arg::String body::Expr
{
  top.expr1 = lamI_c('\', '{', terminal(Name_t, arg, top.location), '}', '.', body.expr1,
    location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production pi
top::Expr ::= arg::Maybe<String> ty::Expr body::Expr
{
  top.expr1 = case arg of
  | just(a) -> pi_c('Pi', terminal(Name_t, a, top.location), ':', ty.expr2,
      '.', body.expr1, location=top.location)
  | nothing() -> arr_c(ty.expr2, '->', body.expr1, location=top.location)
  end;
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production piI
top::Expr ::= arg::String ty::Expr body::Expr
{
  top.expr1 = piI_c('Pi', '{', terminal(Name_t, arg, top.location), ':', ty.expr2,
    '}', '.', body.expr1, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production tyAnnot
top::Expr ::= l::Expr r::Expr
{
  top.expr1 = tyAnnot_c(l.expr2, ':', r.expr1, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production add
top::Expr ::= l::Expr r::Expr
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = add_c(l.expr2, '+', r.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production mul
top::Expr ::= l::Expr r::Expr
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = mul_c(l.expr3, '*', r.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production app
top::Expr ::= l::Expr r::Expr
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = app_c(l.expr4, r.expr5, location=top.location);
  top.expr5 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production appI
top::Expr ::= l::Expr name::String r::Expr
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = appI_c(l.expr4, '{', terminal(Name_t, name, top.location),
    '=', r.expr5, '}', location=top.location);
  top.expr5 = parens_c('(', top.expr1, ')', location=top.location);
}

-- Literal and Identifier productions.

aspect production substVar
top::Expr ::= id::Integer
{
  local name::String = "?" ++ toString(id);
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = var_c(terminal(Name_t, name, top.location), location=top.location);
}

aspect production var
top::Expr ::= name::String
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = var_c(terminal(Name_t, name, top.location), location=top.location);
}

aspect production typeKind
top::Expr ::=
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = expr45_c(top.expr5, location=top.location);
  top.expr5 = typeKind_c(terminal(TypeKind_t, "TYPE", top.location), location=top.location);
}
