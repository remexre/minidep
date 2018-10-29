grammar edu:umn:cs:melt:minidep:abstractsyntax:explicit;

import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

autocopy attribute env :: [Pair<String Maybe<Signature>>];

nonterminal Decls with env, errors, pp;

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

closed nonterminal Implicits with asList<Pair<String Expr>>, env, errors, location, names;
synthesized attribute names :: [String];

abstract production implicitsCons
top::Implicits ::= n::String e::Expr t::Implicits
{
  top.asList = cons(pair(n, e), t.asList);
  top.errors := e.errors ++ t.errors;
  top.names = cons(n, t.names);
}

abstract production implicitsNil
top::Implicits ::=
{
  top.asList = nil();
  top.errors := [];
  top.names = nil();
}

closed nonterminal ImplicitVals with asList<Pair<String Expr>>, env, errors, location;

abstract production implicitValsCons
top::Implicits ::= n::String e::Expr t::Implicits
{
  top.asList = cons(pair(n, e), t.asList);
  top.errors := e.errors ++ t.errors;
}

abstract production implicitValsNil
top::Implicits ::=
{
  top.asList = nil();
  top.errors := [];
}

nonterminal Decl with env, errors, location, pp, sig;
synthesized attribute sig :: Signature;

abstract production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  top.errors := implicits.errors ++ ty.errors ++ body.errors;
  top.sig = sig(name, implicits, ty);
}

closed nonterminal Signature with env;

abstract production sig
top::Signature ::= name::String implicits::Implicits ty::Expr
{}

nonterminal Expr with env, errors, location;

abstract production app
top::Expr ::= f::Expr x::Expr
{
  top.errors := f.errors ++ x.errors;
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
top::Expr ::= name::String implicits::Implicits
{
  top.errors := [];
}
