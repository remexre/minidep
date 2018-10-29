grammar edu:umn:cs:melt:minidep:abstractsyntax:implicit;

import edu:umn:cs:melt:minidep:concretesyntax;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

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

aspect production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  local name_t :: Name_t = terminal(Name_t, name, top.location);
  top.decls_c = declsConsClaim_c(name_t, implicits.implicitTys_c, ':',
    ty.expr1_c, ';;',
    declsConsDef_c(name_t, '=', body.expr1_c, ';;',
      declsNil_c(location=top.location),
      location=top.location),
    location=top.location);
}

synthesized attribute implicitTys_c :: ImplicitTys_c occurs on Implicits;
synthesized attribute implicitTyList_c :: ImplicitTyList_c occurs on Implicits;
synthesized attribute implicitVal_c :: ImplicitVal_c occurs on Implicits;
synthesized attribute implicitVals_c :: ImplicitVals_c occurs on Implicits;

aspect production implicitsCons
top::Implicits ::= n::String e::Expr t::Implicits
{
  local name :: Name_t = terminal(Name_t, n, top.location);
  local ty :: ImplicitTy_c = implicitTy_c(name, ':', e.expr1_c);
  local val :: ImplicitVal_c = implicitVal_c(name, '=', e.expr1_c);
  top.implicitTys_c = implicitTysSome_c('{', ty, t.implicitTyList_c, '}', location=top.location);
  top.implicitTyList_c = implicitTysCons_c(',', ty, t.implicitTyList_c, location=top.location);
  top.implicitVal_c = implicitVal_c(name, '=', e.expr1_c);
  top.implicitVals_c = implicitValsCons_c(',', top.implicitVal_c, t.implicitVals_c,
                                          location=top.location);
}

aspect production implicitsNil
top::Implicits ::=
{
  top.implicitTys_c = implicitTysNone_c(location=top.location);
  top.implicitTyList_c = implicitTysNil_c(location=top.location);
  top.implicitVal_c = error("implicitsNil has no implicitVal_c");
  top.implicitVals_c = implicitValsNil_c(location=top.location);
}

synthesized attribute expr1_c :: Expr1_c occurs on Expr;
synthesized attribute expr2_c :: Expr2_c occurs on Expr;
synthesized attribute expr3_c :: Expr3_c occurs on Expr;
synthesized attribute expr4_c :: Expr4_c occurs on Expr;
synthesized attribute expr5_c :: Expr5_c occurs on Expr;

aspect production app
top::Expr ::= l::Expr r::Expr
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = app_c(l.expr4_c, r.expr5_c, location=top.location);
  top.expr5_c = parens_c('(', top.expr1_c, ')', location=top.location);
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  top.expr1_c = lam_c('\', terminal(Name_t, name, top.location), '.',
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
  | just(name) -> pi_c('Pi', terminal(Name_t, name, top.location),
      ':', l.expr2_c, '.', r.expr1_c, location=top.location)
  | nothing() -> arr_c(l.expr2_c, '->', r.expr1_c, location=top.location)
  end;
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = parens_c('(', top.expr1_c, ')', location=top.location);
}

aspect production var
top::Expr ::= name::String implicits::Implicits
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  
  local name_t :: Name_t = terminal(Name_t, name, top.location);
  top.expr5_c = case implicits of
  | implicitsCons(_, _, t) -> varImplicits_c(name_t, '{', implicits.implicitVal_c,
      t.implicitVals_c, '}', location=top.location)
  | implicitsNil() -> var_c(name_t, location=top.location)
  end;
}
