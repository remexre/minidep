grammar edu:umn:cs:melt:minidep:abstractsyntax:implicit;

import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:util:raw:treeset as set;

autocopy attribute env :: [Pair<String Maybe<Decorated Signature>>];

nonterminal Decls with asList<Decl>, env, errors, pp;

abstract production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.asList = h :: t.asList;
  t.env = mapSndJust(reverse(h.sigs)) ++ top.env;

  top.errors := h.errors ++ t.errors;
}

abstract production declsNil
top::Decls ::=
{
  top.asList = nil();
  top.errors := [];
}

nonterminal Signature with env;

abstract production sig
top::Signature ::= implicits::Implicits ty::Expr
{}

nonterminal Decl with env, errors, location, sigs;
synthesized attribute sigs :: [Pair<String Decorated Signature>];

abstract production declDecl
top::Decl ::= name::String implicits::Implicits ty::Expr
{
  top.errors := implicits.errors ++ ty.errors;
  top.sigs = [pair(name, decorate sig(implicits, ty) with { env = top.env; })];
}

abstract production declDef
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  top.errors := implicits.errors ++ ty.errors ++ body.errors;
  top.sigs = [pair(name, decorate sig(implicits, ty) with { env = top.env; })];
}

nonterminal Implicits with asList<Pair<String Expr>>, env, errors, location, names, nameSet, sorted;
synthesized attribute names :: [String];
synthesized attribute nameSet :: set:Set<String>;
synthesized attribute sorted :: [Pair<String Expr>];

aspect default production
top::Implicits ::=
{
  top.nameSet = set:add(top.names, set:empty(compareString));
}

abstract production implicitsCons
top::Implicits ::= n::String e::Expr t::Implicits
{
  top.asList = pair(n, e) :: t.asList;
  top.errors := e.errors ++ t.errors;
  top.errors <- if set:contains(n, t.nameSet) then
    [err(top.location, "Duplicate implicit " ++ n)]
  else [];
  top.names = n::t.names;
  top.sorted = case t of
  | implicitsCons(n2, e2, t2) ->
      if n < n2
      then pair(n, e) :: t.sorted
      else pair(n2, e2) :: implicitsCons(n, e, t2, location=top.location).sorted
  | implicitsNil() -> [pair(n, e)]
  end;
}

abstract production implicitsNil
top::Implicits ::=
{
  top.asList = nil();
  top.errors := [];
  top.names = [];
  top.sorted = [];
}

nonterminal Expr with env, errors, location;

abstract production anon
top::Expr ::=
{
  top.errors := [];
}

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

abstract production universe
top::Expr ::=
{
  top.errors := [];
}

abstract production var
top::Expr ::= name::String implicits::Implicits
{
  top.errors := [];
}
