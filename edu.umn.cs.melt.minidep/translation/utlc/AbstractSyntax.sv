grammar edu:umn:cs:melt:minidep:translation:utlc;

import edu:umn:cs:melt:minidep:util;
import silver:langutil;

nonterminal Root with errors, pp;

abstract production root
top::Root ::= decls::Decls
{
  top.errors := decls.errors;
}

nonterminal Decls with errors, pp;

abstract production declsCons
top::Decls ::= n::String e::Expr t::Decls
{
  top.errors := e.errors ++ t.errors;
}

abstract production declsNil
top::Decls ::=
{
  top.errors := [];
}

nonterminal Expr with errors, location, pp;

abstract production app
top::Expr ::= f::Expr x::Expr
{
  top.errors := nestErrors(top.location, top.pp, f.errors ++ x.errors);
}

abstract production lam
top::Expr ::= body::Expr
{
  top.errors := nestErrors(top.location, top.pp, body.errors);
}

abstract production globalVar
top::Expr ::= s::String
{
  top.errors := [];
}

abstract production localVar
top::Expr ::= n::Integer
{
  top.errors := [];
}
