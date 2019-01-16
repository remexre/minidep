grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

import edu:umn:cs:melt:minidep:abstractsyntax:explicit as explicit;
import edu:umn:cs:melt:minidep:concretesyntax;
import silver:langutil;
import silver:langutil:pp;

synthesized attribute asExplicit<a> :: a;

attribute asExplicit<explicit:Root> occurs on Root;

function listToDecls
explicit:Decls ::= ds::[explicit:Decl]
{
  return case ds of
  | h::t -> explicit:declsCons(h, listToDecls(t))
  | [] -> explicit:declsNil()
  end;
}

aspect production root
top::Root ::= deps::[Pair<String Root>] decls::Decls
{
  top.asExplicit = explicit:root(listToDecls(flatMap(
    \p::Pair<String Root> -> p.snd.decls.asExplicit, deps)
    ++ decls.asExplicit));
}

attribute asExplicit<[explicit:Decl]> occurs on Decls;

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.asExplicit = h.asExplicit :: t.asExplicit;
}

aspect production declsNil
top::Decls ::=
{
  top.asExplicit = [];
}

attribute asExplicit<explicit:Decl> occurs on Decl;

aspect production declDecl
top::Decl ::= name::String ty::Expr
{
  top.asExplicit = explicit:declDecl(name, ty.asExplicit, location=top.location);
}

aspect production declDef
top::Decl ::= name::String ty::Expr body::Expr
{
  top.asExplicit = explicit:declDef(name, ty.asExplicit, body.asExplicit, location=top.location);
}

attribute asExplicit<explicit:Expr> occurs on Expr;

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.asExplicit = explicit:app(f.asExplicit, x.asExplicit, location=top.location);
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  local argTyExplicit :: explicit:Expr = case top.synTy of
  | pi(_, t, _) -> t.asExplicit
  | _ -> error("lam.synTy not a pi type?")
  end;
  top.asExplicit = explicit:lam(name, argTyExplicit, body.asExplicit, location=top.location);
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.asExplicit = explicit:pi(name, l.asExplicit, r.asExplicit, location=top.location);
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.asExplicit = error("A unification variable can't be made explicit");
}

aspect production universe
top::Expr ::=
{
  top.asExplicit = explicit:universe(location=top.location);
}

aspect production var
top::Expr ::= s::String
{
  top.asExplicit = explicit:var(s, location=top.location);
}
