grammar edu:umn:cs:melt:minidep:abstractsyntax:explicit;

import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

nonterminal Root with errors, pp;

abstract production root
top::Root ::= decls::Decls
{
  top.errors := decls.errors;
}

nonterminal Decls with errors, pp;

abstract production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.errors := h.errors ++ t.errors;
}

abstract production declsNil
top::Decls ::=
{
  top.errors := [];
}

nonterminal Decl with errors, location;

abstract production declDecl
top::Decl ::= name::String ty::Expr
{
  top.errors := ty.errors;
}

abstract production declDef
top::Decl ::= name::String ty::Expr body::Expr
{
  top.errors := ty.errors ++ body.errors;
}

nonterminal Expr with errors, location;

abstract production app
top::Expr ::= f::Expr x::Expr
{
  top.errors := nestErrors(top.location, top.pp, f.errors ++ x.errors);
}

abstract production lam
top::Expr ::= name::String argTy::Expr body::Expr
{
  top.errors := nestErrors(top.location, top.pp, argTy.errors ++ body.errors);
}

abstract production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.errors := nestErrors(top.location, top.pp, l.errors ++ r.errors);
}

abstract production universe
top::Expr ::=
{
  top.errors := [];
}

abstract production var
top::Expr ::= s::String
{
  top.errors := [];
}
