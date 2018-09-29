grammar edu:umn:cs:melt:minidep:abstractsyntax;

import edu:umn:cs:melt:minidep:concretesyntax;

synthesized attribute expr1 :: Expr1_c;
synthesized attribute expr2 :: Expr2_c;
synthesized attribute expr3 :: Expr3_c;
synthesized attribute expr4 :: Expr4_c;

aspect production tyAnnot
top::Expr ::= l::Expr r::Expr
{
  top.expr1 = tyAnnot_c(l.expr1, ':', r.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production add
top::Expr ::= l::Expr r::Expr
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = add_c(l.expr2, '+', r.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = parens_c('(', top.expr1, ')', location=top.location);
}

aspect production mul
top::Expr ::= l::Expr r::Expr
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = mul_c(l.expr3, '*', r.expr4, location=top.location);
  top.expr4 = parens_c('(', top.expr1, ')', location=top.location);
}

-- Literal and Identifier productions.

aspect production nat
top::Expr ::= n::Integer -- RIP no unsigned ints
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = nat_c(terminal(Nat_t, toString(n), top.location), location=top.location);
}

aspect production natTy
top::Expr ::=
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = natTy_c('Nat', location=top.location);
}

aspect production typeKind
top::Expr ::=
{
  top.expr1 = expr12_c(top.expr2, location=top.location);
  top.expr2 = expr23_c(top.expr3, location=top.location);
  top.expr3 = expr34_c(top.expr4, location=top.location);
  top.expr4 = typeKind_c('TYPE', location=top.location);
}
