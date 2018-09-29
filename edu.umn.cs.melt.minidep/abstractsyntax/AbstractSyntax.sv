grammar edu:umn:cs:melt:minidep:abstractsyntax;

import edu:umn:cs:melt:minidep:concretesyntax;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

nonterminal Root with env, errors, location, pp;
nonterminal Expr with env, errors, location;

inherited attribute env :: [Pair<String Pair<Expr Expr>>];
synthesized attribute errors :: [Message] with ++;

abstract production root
top::Root ::= e::Expr
{
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

abstract production pi
top::Expr ::= arg::Maybe<String> ty::Expr body::Expr
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

-- Literal and Identifier productions.

abstract production var
top::Expr ::= name::Maybe<String>
{
  top.errors := [];
}

abstract production nat
top::Expr ::= n::Integer -- RIP no unsigned ints
{
  top.errors := if n < 0
    then [err(top.location, "Somehow a negative number got in here?")]
    else [];
}

abstract production natTy
top::Expr ::=
{
  top.errors := [];
}

abstract production typeKind
top::Expr ::=
{
  top.errors := [];
}
