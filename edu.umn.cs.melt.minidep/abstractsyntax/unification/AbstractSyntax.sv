grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treeset as set;

nonterminal Root with decls, deps, errors, pp;
synthesized attribute decls :: Decls;
synthesized attribute deps :: [Pair<String Root>];

abstract production root
top::Root ::= deps::[Pair<String Root>] decls::Decls
{
  top.decls = decls;
  top.deps = deps;
  top.errors := decls.errors ++ flatMap(
    \p::Pair<String Root> -> p.snd.errors, deps);
}

nonterminal Decls with errors, pp, synTyEnv, synValEnv;
synthesized attribute synTyEnv :: [Pair<String Maybe<Expr>>];
synthesized attribute synValEnv :: [Pair<String Maybe<Expr>>];

abstract production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.errors := h.errors ++ t.errors;
  top.synTyEnv = t.synTyEnv ++ h.synTyEnv;
  top.synValEnv = t.synValEnv ++ h.synValEnv;
}

abstract production declsNil
top::Decls ::=
{
  top.errors := [];
  top.synTyEnv = [];
  top.synValEnv = [];
}

nonterminal Decl with errors, location, synTyEnv, synValEnv;

abstract production declDecl
top::Decl ::= name::String ty::Expr
{
  top.errors := ty.errors;
  top.synTyEnv = [pair(name, just(ty))];
  top.synValEnv = [pair(name, nothing())];
}

abstract production declDef
top::Decl ::= name::String ty::Expr body::Expr
{
  top.errors := ty.errors ++ body.errors;
  top.synTyEnv = [pair(name, just(ty))];
  top.synValEnv = [pair(name, just(body))];
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
