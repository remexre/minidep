grammar edu:umn:cs:melt:minidep:abstractsyntax:implicit;

import edu:umn:cs:melt:minidep:abstractsyntax:spined as spined;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treemap as rtm;
import silver:util:raw:treemap only Map;
import silver:util:raw:treeset as set;

synthesized attribute elaboratedDecls :: spined:Decls occurs on Decls;

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  t.env = map(
    \p::Pair<String Signature> -> pair(p.fst, just(p.snd)),
    reverse(h.sigs)) ++ top.env;

  top.elaboratedDecls = spined:declsCons(h.elaboratedDecl, t.elaboratedDecls);
}

aspect production declsNil
top::Decls ::=
{
  top.elaboratedDecls = spined:declsNil();
}

synthesized attribute elaboratedDecl :: spined:Decl occurs on Decl;

aspect production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  top.elaboratedDecl = spined:decl(name, implicits.elaboratedImplicits,
                                   ty.elaboratedExpr, body.elaboratedExpr,
                                   location=top.location);
}

synthesized attribute elaboratedImplicits :: Map<String spined:Expr> occurs on Implicits;

aspect default production
top::Implicits ::=
{
  top.elaboratedImplicits = rtm:add(map(
    \p::Pair<String Decorated Expr> -> pair(p.fst, p.snd.elaboratedExpr),
    top.asList), rtm:empty(compareString));
}

synthesized attribute elaboratedExpr :: spined:Expr occurs on Expr;
synthesized attribute asArg :: (spined:Expr ::= [Decorated Expr]) occurs on Expr;

aspect default production
top::Expr ::=
{
  top.asArg = \xs::[Decorated Expr] -> spined:call(top.elaboratedExpr,
    map((.elaboratedExpr), xs), location=top.location);
}

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.asArg = \xs::[Decorated Expr] -> f.asArg(cons(x, xs));
  top.elaboratedExpr = top.asArg([]);
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
  local wanted :: set:Set<String> = case lookupBy(stringEq, name, top.env) of
  | just(just(sig(implicits, _))) -> implicits.names
  | _ -> set:empty(compareString)
  end;

  top.errors <- case set:toList(set:difference(implicits.names, wanted)) of
  | [] -> []
  | [n] -> [err(implicits.location, "Extra implicit: " ++ n)]
  | ns -> [err(implicits.location, "Extra implicits: " ++ implode(", ", ns))]
  end;

  local missingImplicits :: [Pair<String spined:Expr>] = map(
    \n :: String -> pair(n, spined:unificationVar(genInt(), location=top.location)),
    set:toList(set:difference(wanted, implicits.names)));
  local allImplicits :: Map<String spined:Expr> =
    rtm:add(missingImplicits, rtm:add(map(
      \p::Pair<String Decorated Expr> -> pair(p.fst, p.snd.elaboratedExpr),
      implicits.asList), rtm:empty(compareString)));

  top.elaboratedExpr = spined:var(name, allImplicits, location=top.location);
}
