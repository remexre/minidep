grammar edu:umn:cs:melt:minidep:abstractsyntax:implicit;

import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treeset as set;

autocopy attribute env :: [Pair<String Maybe<Signature>>];

closed nonterminal Decls with asList<Decl>, env, errors, pp;

abstract production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.asList = cons(h, t.asList);
  top.errors := h.errors ++ t.errors;
}

abstract production declsNil
top::Decls ::=
{
  top.asList = nil();
  top.errors := [];
}

closed nonterminal Signature with env;

abstract production sig
top::Signature ::= implicits::Implicits ty::Expr
{}

nonterminal Decl with env, errors, location, sigs;
synthesized attribute sigs :: [Pair<String Signature>];

abstract production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  top.errors := implicits.errors ++ ty.errors ++ body.errors;
  top.sigs = [pair(name, sig(implicits, ty))];
}

closed nonterminal Implicits with asList<Pair<String Decorated Expr>>, env, errors, location, names;
synthesized attribute names :: set:Set<String>;

abstract production implicitsCons
top::Implicits ::= n::String e::Expr t::Implicits
{
  top.asList = cons(pair(n, e), t.asList);
  top.errors := e.errors ++ t.errors;

  top.names = set:add([n], t.names);
  top.errors <- if set:contains(n, t.names) then
    [err(top.location, "Duplicate implicit " ++ n)]
  else [];
}

abstract production implicitsNil
top::Implicits ::=
{
  top.asList = nil();
  top.errors := [];
  top.names = set:empty(compareString);
}

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
