grammar edu:umn:cs:melt:minidep:abstractsyntax:implicit;

import edu:umn:cs:melt:minidep:concretesyntax;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

nonterminal Root with atoms, env, errors, location, pp;
nonterminal Expr with atoms, env, errors, location;

-- "Magically defined" values, and their types.
inherited attribute atoms :: [Pair<String Expr>];

-- The declared values in the environment, with their definitions and types.
inherited attribute env :: [Pair<String Pair<Expr Expr>>];

synthesized attribute errors :: [Message] with ++;

abstract production root
top::Root ::= e::Expr
{
  e.atoms = top.atoms;
  e.env = top.env;
  top.errors := e.errors;
  top.pp = ppExpr(e);
}

-- Operator productions.

abstract production lam
top::Expr ::= arg::String body::Expr
{
  body.env = top.env;
  top.errors := body.errors;
}

abstract production lamI
top::Expr ::= arg::String body::Expr
{
  body.env = top.env;
  top.errors := body.errors;
}

abstract production pi
top::Expr ::= arg::Maybe<String> ty::Expr body::Expr
{
  ty.env = top.env;
  body.env = top.env;
  top.errors := ty.errors ++ body.errors;
}

abstract production piI
top::Expr ::= arg::String ty::Expr body::Expr
{
  ty.env = top.env;
  body.env = top.env;
  top.errors := ty.errors ++ body.errors;
}

abstract production tyAnnot
top::Expr ::= l::Expr r::Expr
{
  l.env = top.env;
  r.env = top.env;
  top.errors := l.errors ++ r.errors;
}

abstract production add
top::Expr ::= l::Expr r::Expr
{
  l.env = top.env;
  r.env = top.env;
  top.errors := l.errors ++ r.errors;
}

abstract production mul
top::Expr ::= l::Expr r::Expr
{
  l.env = top.env;
  r.env = top.env;
  top.errors := l.errors ++ r.errors;
}

abstract production app
top::Expr ::= l::Expr r::Expr
{
  l.env = top.env;
  r.env = top.env;
  top.errors := l.errors ++ r.errors;
}

abstract production appI
top::Expr ::= l::Expr arg::String r::Expr
{
  l.env = top.env;
  r.env = top.env;
  top.errors := l.errors ++ r.errors;
}

-- Literal and Identifier productions.

abstract production substVar
top::Expr ::= id::Integer
{
  top.errors := [];
}

abstract production var
top::Expr ::= name::String
{
  top.errors := [];
}

abstract production typeKind
top::Expr ::=
{
  top.errors := [];
}
