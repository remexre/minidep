grammar edu:umn:cs:melt:minidep:abstractsyntax:spined;

import edu:umn:cs:melt:minidep:concretesyntax;
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

function asImplicitTy_c
ImplicitTy_c ::= p::Pair<String Expr> loc::Location
{
  return implicitTy_c(terminal(Name_t, p.fst, loc), ':', p.snd.expr1_c);
}

function asImplicitTyList_c
ImplicitTyList_c ::= l::[Pair<String Expr>] loc::Location
{
  return foldr(
    \p :: Pair<String Expr> imps::ImplicitTyList_c ->
      implicitTysCons_c(',', asImplicitTy_c(p, loc), imps, location=loc),
    implicitTysNil_c(location=loc), l);
}

aspect production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  local name_t :: Name_t = terminal(Name_t, name, top.location);
  local imps :: ImplicitTys_c = if !null(implicits.asList) then
    implicitTysSome_c('{', asImplicitTy_c(head(implicits.asList), top.location),
      asImplicitTyList_c(tail(implicits.asList), top.location), '}', location=top.location)
  else
    implicitTysNone_c(location=top.location);
  top.decls_c = declsConsClaim_c(name_t, ':', imps, ty.expr1_c, ';',
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

function callAsApps
Expr4_c ::= f::Expr4_c xs::Exprs loc::Location
{
  return case xs of
  | exprsCons(hd, tl) -> callAsApps(app_c(f, hd.expr5_c, location=loc), tl, loc)
  | exprsNil() -> f
  end;
}

aspect production call
top::Expr ::= f::String xs::Exprs
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = callAsApps(expr45_c(var_c(terminal(Name_t, f, top.location),
                                          location=top.location),
                                    location=top.location),
                           xs, top.location);
  top.expr5_c = case top.expr4_c of
  | expr45_c(e) -> e
  | _ -> parens_c('(', top.expr1_c, ')', location=top.location)
  end;
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

function asImplicitVals_c
ImplicitVals_c ::= l::[Pair<String Expr>] loc::Location
{
  return foldr(
    \p :: Pair<String Expr> imps::ImplicitVals_c ->
      implicitValsCons_c(',',
        implicitVal_c(terminal(Name_t, p.fst, loc), '=', p.snd.expr1_c),
        imps, location=loc),
    implicitValsNil_c(location=loc), l);
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = var_c(terminal(Name_t, "?" ++ toString(id), top.location),
                      location=top.location);
}
