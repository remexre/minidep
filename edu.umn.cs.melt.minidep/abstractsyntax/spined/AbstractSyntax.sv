grammar edu:umn:cs:melt:minidep:abstractsyntax:spined;

import silver:langutil;

autocopy attribute env :: [Pair<String Maybe<Signature>>];

nonterminal Decls with constraints, env, errors, pp;

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

nonterminal Signature with constraints;

abstract production sig
top::Signature ::= implicits::Implicits ty::Expr
{}

nonterminal Decl with constraints, env, errors, location, sigs;
synthesized attribute sigs :: [Pair<String Signature>];

abstract production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  top.errors := ty.errors ++ body.errors;
  top.errors <- flatMap(
    \p::Pair<String Expr> -> p.snd.errors,
    implicits.asList);
  top.sigs = [pair(name, sig(implicits, ty))];
}

nonterminal Implicits with asList<Pair<String Expr>>, constraints, errors, implicitNames, location;
synthesized attribute asList<a> :: [a];
synthesized attribute implicitNames :: [String];

abstract production implicitsCons
top::Implicits ::= name::String ex::Expr tl::Implicits
{
  top.asList = pair(name, ex) :: tl.asList;
  top.errors := [];
  top.errors <- if containsBy(stringEq, name, tl.implicitNames)
    then [err(top.location, "Duplicate implicit name: " ++ name)]
    else [];
  top.implicitNames = (name :: tl.implicitNames);
}

abstract production implicitsNil
top::Implicits ::=
{
  top.asList = [];
  top.errors := [];
  top.implicitNames = [];
}

nonterminal Exprs with append, asList<Expr>, constraints, env, errors, location;
synthesized attribute append :: (Exprs ::= Expr);

abstract production exprsCons
top::Exprs ::= hd::Expr tl::Exprs
{
  top.append = \e::Expr -> exprsCons(hd, tl.append(e), location=top.location);
  top.asList = hd :: tl.asList;
  top.errors := hd.errors ++ tl.errors;
}

abstract production exprsNil
top::Exprs ::=
{
  top.append = \e::Expr -> exprsCons(e, exprsNil(location=top.location), location=e.location);
  top.asList = [];
  top.errors := [];
}

nonterminal Expr with constraints, env, errors, location;

abstract production call
top::Expr ::= f::String xs::Exprs
{
  top.errors := xs.errors;
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
