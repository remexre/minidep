grammar edu:umn:cs:melt:minidep:abstractsyntax:explicit;

import edu:umn:cs:melt:minidep:concretesyntax;
import silver:langutil;
import silver:langutil:pp;

aspect production root
top::Root ::= decls::Decls
{
  top.pp = decls.pp;
}

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.pp = ppConcat([h.decls_c.pp, line(), t.pp]);
}

aspect production declsNil
top::Decls ::=
{
  top.pp = notext();
}

synthesized attribute decls_c :: Decls_c occurs on Decl;

aspect production declDecl
top::Decl ::= name::String ty::Expr
{
  top.decls_c = declsConsClaim_c(terminal(Name_t, name, top.location), ':',
                                 implicitTysNone_c(location=top.location), ty.expr1_c,
                                 ';',
                                 declsNil_c(location=top.location), location=top.location);
}

aspect production declDef
top::Decl ::= name::String ty::Expr body::Expr
{
  local name_t :: Name_t = terminal(Name_t, name, top.location);
  top.decls_c = declsConsClaim_c(name_t, ':', implicitTysNone_c(location=top.location), ty.expr1_c,
    ';',
    declsConsDef_c(name_t, '=', body.expr1_c, ';',
      declsNil_c(location=top.location),
      location=top.location),
    location=top.location);
}

synthesized attribute expr1_c :: Expr1_c occurs on Expr;
synthesized attribute expr2_c :: Expr2_c occurs on Expr;
synthesized attribute expr3_c :: Expr3_c occurs on Expr;
synthesized attribute expr4_c :: Expr4_c occurs on Expr;
synthesized attribute expr5_c :: Expr5_c occurs on Expr;
attribute pp occurs on Expr;

aspect default production
top::Expr ::=
{
  top.pp = parens(top.expr1_c.pp);
}

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = app_c(f.expr4_c, x.expr5_c, location=top.location);
  top.expr5_c = parens_c('(', top.expr1_c, ')', location=top.location);
}

aspect production lam
top::Expr ::= name::String argTy::Expr body::Expr
{
  top.expr1_c = lam_c('\', argsNilArg_c(terminal(Name_t, name, top.location)), '->',
    body.expr1_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = parens_c('(', top.expr1_c, ')', location=top.location);
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.expr1_c = case name of
  | just(name) -> pi_c('(', terminal(Name_t, name, top.location),
      ':', l.expr1_c, ')', '->', r.expr1_c, location=top.location)
  | nothing() -> arr_c(l.expr2_c, '->', r.expr1_c, location=top.location)
  end;
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = parens_c('(', top.expr1_c, ')', location=top.location);
}

aspect production universe
top::Expr ::=
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = type_c('TYPE', location=top.location);
}

aspect production var
top::Expr ::= s::String
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = var_c(terminal(Name_t, s, top.location), location=top.location);
}
