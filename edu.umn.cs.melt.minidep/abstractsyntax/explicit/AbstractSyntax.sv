grammar edu:umn:cs:melt:minidep:abstractsyntax:explicit;

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

nonterminal Implicits with asList<Pair<String Expr>>, errors, location;
synthesized attribute asList<a> :: [a];

abstract production implicitsCons
top::Implicits ::= n::String e::Expr t::Implicits
{
  top.asList = cons(pair(n, e), t.asList);
  top.errors := e.errors ++ t.errors;
}

abstract production implicitsNil
top::Implicits ::=
{
  top.asList = nil();
  top.errors := [];
}

nonterminal Decl with errors, location, pp, sig;
synthesized attribute sig :: Signature;

abstract production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  top.errors := implicits.errors ++ ty.errors ++ body.errors;
  top.sig = sig(name, implicits, ty);
}

nonterminal Signature;

abstract production sig
top::Signature ::= name::String implicits::Implicits ty::Expr
{}

nonterminal Expr with errors, location;

abstract production app
top::Expr ::= l::Expr r::Expr
{
  top.errors := l.errors ++ r.errors;
}

abstract production lam
top::Expr ::= name::String body::Expr
{
  top.errors := body.errors;
}

abstract production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.errors := l.errors ++ r.errors;
}

abstract production var
top::Expr ::= name::String
{
  top.errors := [];
}
