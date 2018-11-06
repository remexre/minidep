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
  top.elaboratedExpr = implicits.appTo(top.env, ty.elaboratedExpr);
}

synthesized attribute elaboratedDecl :: unification:Decl occurs on Decl;

aspect production declDecl
top::Decl ::= name::String implicits::Implicits ty::Expr
{
  top.elaboratedDecl = unification:declDecl(name, implicits.appTo(top.env, ty.elaboratedExpr),
                                            location=top.location);
}

aspect production declDef
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  top.elaboratedDecl = unification:declDef(name, implicits.appTo(top.env, ty.elaboratedExpr),
                                            body.elaboratedExpr, location=top.location);
}

synthesized attribute appTo :: (unification:Expr ::= [Pair<String Maybe<Decorated Signature>>] unification:Expr) occurs on Implicits;
synthesized attribute find :: (Maybe<Expr> ::= String) occurs on Implicits;

aspect default production
top::Implicits ::=
{
  top.appTo = \env::[Pair<String Maybe<Decorated Signature>>] ex::unification:Expr -> foldr(
    \h::Pair<String Expr> t::unification:Expr ->
      unification:pi(just(h.fst), (decorate h.snd with { env = env; }).elaboratedExpr, t,
                     location=top.location),
    ex, top.sorted);
}

aspect production implicitsCons
top::Implicits ::= n::String e::Expr t::Implicits
{
  top.find = \s::String -> if n == s then just(e) else t.find(s);
}

aspect production implicitsNil
top::Implicits ::=
{
  top.find = \s::String -> nothing();
}

aspect production anon
top::Expr ::=
{
  top.elaboratedExpr = unification:unificationVar(genInt(), location=top.location);
}

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.elaboratedExpr = unification:app(f.elaboratedExpr, x.elaboratedExpr, location=top.location);
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  body.env = pair(name, nothing()) :: top.env;

  top.elaboratedExpr = unification:lam(name, body.elaboratedExpr, location=top.location);
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  r.env = case name of
  | just(n) -> pair(n, nothing()) :: top.env
  | nothing() -> top.env
  end;

  top.elaboratedExpr = unification:pi(name, l.elaboratedExpr, r.elaboratedExpr, location=top.location);
}

aspect production universe
top::Expr ::=
{
  top.elaboratedExpr = unification:universe(location=top.location);
}

aspect production var
top::Expr ::= name::String implicits::Implicits
{
  local wanted :: [String] = case lookupTyEnv(name, top.env) of
  | just(sig(implicits, _)) -> implicits.names
  | _ -> []
  end;

  top.errors <- case set:toList(set:difference(implicits.nameSet, set:add(wanted, set:empty(compareString)))) of
  | [] -> []
  | [n] -> [err(implicits.location, "Extra implicit: " ++ n)]
  | ns -> [err(implicits.location, "Extra implicits: " ++ implode(", ", ns))]
  end;

  top.elaboratedExpr = foldr(
    \n::String f::unification:Expr ->
      let
        arg :: unification:Expr = case implicits.find(n) of
                                  | just(e) -> (decorate e with { env = top.env; }).elaboratedExpr
                                  | nothing() -> unification:unificationVar(genInt(), location=top.location)
                                  end
      in
        unification:app(f, arg, location=top.location)
      end,
    unification:var(name, location=top.location),
    sortBy(stringLte, wanted));
}
