grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

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

abstract production decl
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
top::Expr ::= name::String body::Expr
{
  top.errors := nestErrors(top.location, top.pp, body.errors);
}

abstract production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.errors := nestErrors(top.location, top.pp, l.errors ++ r.errors);
}

abstract production unificationVar
top::Expr ::= id::Integer
{
  top.errors := [];
}

abstract production var
top::Expr ::= s::String
{
  top.errors := [];
}
