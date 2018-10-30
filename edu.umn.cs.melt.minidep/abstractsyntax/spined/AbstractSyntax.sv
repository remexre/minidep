grammar edu:umn:cs:melt:minidep:abstractsyntax:spined;

import silver:langutil;
import silver:util:raw:treemap as rtm;
import silver:util:raw:treemap only Map;

autocopy attribute env :: [Pair<String Maybe<Signature>>];

closed nonterminal Decls with env, errors, pp;

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

closed nonterminal Signature;

abstract production sig
top::Signature ::= implicits::Map<String Expr> ty::Expr
{}

nonterminal Decl with env, errors, location, sigs;
synthesized attribute sigs :: [Pair<String Signature>];

abstract production decl
top::Decl ::= name::String implicits::Map<String Expr> ty::Expr body::Expr
{
  top.errors := ty.errors ++ body.errors;
  top.errors <- flatMap(
    \p::Pair<String Expr> -> p.snd.errors,
    rtm:toList(implicits));
  top.sigs = [pair(name, sig(implicits, ty))];
}

closed nonterminal Expr with env, errors, location;

abstract production call
top::Expr ::= f::Expr xs::[Expr]
{
  top.errors := f.errors ++ flatMap((.errors), xs);
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

abstract production unificationVar
top::Expr ::= id::Integer
{
  top.errors := [];
}

abstract production var
top::Expr ::= name::String implicits::Map<String Expr>
{
  top.errors := [];
}
