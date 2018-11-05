grammar edu:umn:cs:melt:minidep:abstractsyntax:implicit;

import edu:umn:cs:melt:minidep:abstractsyntax:spined as spined;
import edu:umn:cs:melt:minidep:abstractsyntax:spined only append, sorted;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:util:raw:treeset as set;

synthesized attribute elaboratedDecls :: spined:Decls occurs on Decls;

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.elaboratedDecls = spined:declsCons(h.elaboratedDecl, t.elaboratedDecls);
}

aspect production declsNil
top::Decls ::=
{
  top.elaboratedDecls = spined:declsNil();
}

synthesized attribute elaboratedExpr :: spined:Expr occurs on Expr, Signature;

aspect production sig
top::Signature ::= implicits::Implicits ty::Expr
{
  top.elaboratedExpr = foldr(
    \p::Pair<String Expr> t::spined:Expr ->
      spined:pi(just(p.fst), p.snd.elaboratedExpr, t, location=ty.location),
    ty.elaboratedExpr,
    sortBy(
      \l::Pair<String Expr> r::Pair<String Expr> -> stringLte(l.fst, r.fst),
      implicits.asList
    ));
}

synthesized attribute elaboratedDecl :: spined:Decl occurs on Decl;

aspect production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  top.elaboratedDecl = spined:decl(name, implicits.elaboratedImplicits,
                                   ty.elaboratedExpr, body.elaboratedExpr,
                                   location=top.location);
}

synthesized attribute elaboratedImplicits :: spined:Implicits occurs on Implicits;

aspect production implicitsCons
top::Implicits ::= n::String e::Expr t::Implicits
{
  top.elaboratedImplicits = spined:implicitsCons(n, e.elaboratedExpr,
    t.elaboratedImplicits, location=top.location);
}

aspect production implicitsNil
top::Implicits ::=
{
  top.elaboratedImplicits = spined:implicitsNil(location=top.location);
}

aspect production app
top::Expr ::= f::Expr x::Expr
{
  local e :: spined:Expr = f.elaboratedExpr;
  top.elaboratedExpr = case e of
  | spined:call(f, xs) -> spined:call(f, xs.append(x.elaboratedExpr), location=e.location)
  end;
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  body.env = cons(pair(name, nothing()), top.env);

  top.elaboratedExpr = spined:lam(name, body.elaboratedExpr, location=top.location);
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  r.env = case name of
  | just(n) -> cons(pair(n, nothing()), top.env)
  | nothing() -> top.env
  end;

  top.elaboratedExpr = spined:pi(name, l.elaboratedExpr, r.elaboratedExpr, location=top.location);
}

aspect production var
top::Expr ::= name::String implicits::Implicits
{
  local wanted :: set:Set<String> = case lookupTyEnv(name, top.env) of
  | just(sig(implicits, _)) -> implicits.names
  | _ -> set:empty(compareString)
  end;

  top.errors <- case set:toList(set:difference(implicits.names, wanted)) of
  | [] -> []
  | [n] -> [err(implicits.location, "Extra implicit: " ++ n)]
  | ns -> [err(implicits.location, "Extra implicits: " ++ implode(", ", ns))]
  end;

  local allImplicits :: spined:Exprs = foldr(
    \n::String tl::spined:Implicits ->
      spined:implicitsCons(n, spined:unificationVar(genInt(), location=top.location),
                           tl, location=top.location),
    implicits.elaboratedImplicits,
    set:toList(set:difference(wanted, implicits.names))).sorted;

  top.elaboratedExpr = spined:call(name, allImplicits, location=top.location);
}
