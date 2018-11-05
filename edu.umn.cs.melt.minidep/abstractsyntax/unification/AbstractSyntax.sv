grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treeset as set;

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

nonterminal Expr with errors, fv, location;
synthesized attribute fv :: set:Set<String>;

abstract production app
top::Expr ::= f::Expr x::Expr
{
  top.errors := nestErrors(top.location, top.pp, f.errors ++ x.errors);
  top.fv = set:union(f.fv, x.fv);
}

abstract production lam
top::Expr ::= name::String body::Expr
{
  top.errors := nestErrors(top.location, top.pp, body.errors);
  top.fv = set:difference(body.fv, set1(name));
}

abstract production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.errors := nestErrors(top.location, top.pp, l.errors ++ r.errors);
  top.fv = case name of
  | just(n) -> set:union(l.fv, set:difference(r.fv, set1(n)))
  | nothing() -> set:union(l.fv, r.fv)
  end;
}

abstract production unificationVar
top::Expr ::= id::Integer
{
  top.errors := [];
  top.fv = set:empty(compareString);
}

abstract production universe
top::Expr ::=
{
  top.errors := [];
  top.fv = set:empty(compareString);
}

abstract production var
top::Expr ::= s::String
{
  top.errors := [];
  top.fv = set1(s);
}
