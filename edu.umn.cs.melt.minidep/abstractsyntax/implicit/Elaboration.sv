grammar edu:umn:cs:melt:minidep:abstractsyntax:implicit;

import edu:umn:cs:melt:minidep:abstractsyntax:unification as unification;
import edu:umn:cs:melt:minidep:abstractsyntax:unification only append;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:util:raw:treeset as set;

synthesized attribute elaboratedDecls :: unification:Decls occurs on Decls;

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.elaboratedDecls = unification:declsCons(h.elaboratedDecl, t.elaboratedDecls);
}

aspect production declsNil
top::Decls ::=
{
  top.elaboratedDecls = unification:declsNil();
}

synthesized attribute elaboratedExpr :: unification:Expr occurs on Expr, Signature;

aspect production sig
top::Signature ::= implicits::Implicits ty::Expr
{
  top.elaboratedExpr = foldr(
    \h::Pair<String Expr> t::unification:Expr ->
      unification:pi(
        just(h.fst),
        decorate h.snd with {
          env = top.env;
        }.elaboratedExpr,
        t,
        location=t.location),
    ty.elaboratedExpr, implicits.sorted);
}

synthesized attribute elaboratedDecl :: unification:Decl occurs on Decl;

aspect production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  local s :: Decorated Signature = decorate sig(implicits, ty) with {
    env = top.env;
  };
  top.elaboratedDecl = unification:decl(name, s.elaboratedExpr,
    body.elaboratedExpr, location=top.location);
}

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.elaboratedExpr = unification:app(f.elaboratedExpr, x.elaboratedExpr, location=top.location);
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  body.env = cons(pair(name, nothing()), top.env);

  top.elaboratedExpr = unification:lam(name, body.elaboratedExpr, location=top.location);
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  r.env = case name of
  | just(n) -> cons(pair(n, nothing()), top.env)
  | nothing() -> top.env
  end;

  top.elaboratedExpr = unification:pi(name, l.elaboratedExpr, r.elaboratedExpr, location=top.location);
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

  top.elaboratedExpr = foldr(
    \n::String f::unification:Expr ->
      unification:app(f, unification:unificationVar(genInt(), location=top.location),
                      location=top.location),
    unification:var(name, location=top.location),
    sortBy(stringLte, set:toList(set:difference(wanted, implicits.names))));
}
