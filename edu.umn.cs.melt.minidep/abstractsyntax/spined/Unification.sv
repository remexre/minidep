grammar edu:umn:cs:melt:minidep:abstractsyntax:spined;

import edu:umn:cs:melt:minidep:concretesyntax only pp;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

nonterminal Subst;

abstract production substVar
top::Subst ::= var::Integer expr::Expr
{}

closed nonterminal Constraint with pp, solve;
synthesized attribute solve :: Pair<[Constraint] Maybe<Subst>>;

abstract production constraintEq
top::Constraint ::= l::Expr r::Expr
{
  top.pp = ppConcat(
    [ l.expr5_c.pp
    , text(" ~ ")
    , r.expr5_c.pp
    ]);
  top.solve = if l.eqExpr(r)
    then pair([], nothing())
    else case l, r of
    | unificationVar(l), _ -> pair([], just(substVar(l, r)))
    | _, unificationVar(_) -> pair([constraintEq(r, l)], nothing())
    | _, _ -> error("TODO Solve " ++ show(80, top.pp))
    end;
}

autocopy attribute inhTyEnv :: [Pair<String Maybe<Expr>>] occurs on Decl, Decls, Expr, Exprs,
                                                                    Implicits;
synthesized attribute constraints :: [Constraint] with ++;
attribute constraints occurs on Decl, Decls, Expr, Exprs, Implicits;
synthesized attribute hasVars :: Boolean occurs on Decl, Decls, Expr, Exprs;

synthesized attribute substDecls :: (Decls ::= Subst) occurs on Decls;
synthesized attribute unified :: Decls occurs on Decls;

function solveAll
Decls ::= cs::[Constraint] ds::Decls
{
  return case cs of
  | h::t -> solveAll(h.solve.fst ++ t,
      case h.solve.snd of
      | just(s) -> ds.substDecls(s)
      | nothing() -> ds
      end)
  | [] -> ds
  end;
}

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.constraints := h.constraints ++ t.constraints;
  top.hasVars = h.hasVars || t.hasVars;
  top.unified = solveAll(top.constraints, top);

  top.substDecls = \s::Subst -> declsCons(h.substDecl(s), t.substDecls(s));
}

aspect production declsNil
top::Decls ::=
{
  top.constraints := [];
  top.hasVars = false;
  top.unified = declsNil();

  top.substDecls = \s::Subst -> declsNil();
}

function implicitsHaveVars
Boolean ::= implicits::Implicits
{
  return foldr(\p::Pair<String Expr> has::Boolean -> has || p.snd.hasVars,
    false, implicits.asList);
}

synthesized attribute substDecl :: (Decl ::= Subst) occurs on Decl;

aspect production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  ty.inhTy = nothing();
  body.inhTy = just(ty);
  local tyEnv :: [Pair<String Maybe<Expr>>] = mapSndJust(implicits.asList) ++ top.inhTyEnv;
  ty.inhTyEnv = tyEnv;
  body.inhTyEnv = tyEnv;

  top.constraints := implicits.constraints ++ ty.constraints ++ body.constraints;
  top.hasVars = implicitsHaveVars(implicits) || ty.hasVars || body.hasVars;

  top.substDecl = \s::Subst -> decl(name, implicits.substImplicits(s), ty.substExpr(s),
    body.substExpr(s), location=top.location);
}

synthesized attribute substImplicits :: (Implicits ::= Subst) occurs on Implicits;

aspect production implicitsCons
top::Implicits ::= name::String ex::Expr tl::Implicits
{
  ex.inhTy = nothing();
  top.constraints := ex.constraints ++ tl.constraints;

  top.substImplicits = \s::Subst ->
    implicitsCons(name, ex.substExpr(s), tl.substImplicits(s), location=top.location);
}

aspect production implicitsNil
top::Implicits ::=
{
  top.constraints := [];

  top.substImplicits = \s::Subst -> implicitsNil(location=top.location);
}

inherited attribute inhTys :: [Maybe<Expr>] occurs on Exprs;
synthesized attribute substExprs :: (Exprs ::= Subst) occurs on Exprs;
synthesized attribute synTys :: Exprs occurs on Exprs;

aspect production exprsCons
top::Exprs ::= hd::Expr tl::Exprs
{
  top.constraints := hd.constraints ++ tl.constraints;
  top.hasVars = hd.hasVars || tl.hasVars;

  hd.inhTy = case top.inhTys of
  | h::_ -> h
  | [] -> nothing()
  end;
  tl.inhTys = case top.inhTys of
  | _::t -> t
  | [] -> []
  end;
  top.synTys = exprsCons(hd.synTy, tl.synTys, location=builtin());

  top.substExprs = \s::Subst ->
    exprsCons(hd.substExpr(s), tl.substExprs(s), location=top.location);
}

function ppMExpr
Document ::= e::Maybe<Expr>
{ return case e of | just(e) -> e.pp | nothing() -> text("nothing!") end; }

aspect production exprsNil
top::Exprs ::=
{
  top.constraints := [];
  top.hasVars = false;

  top.errors <- case top.inhTys of
  | h::t -> nestErrors(top.location, ppImplode(text(", "), map(ppMExpr, top.inhTys)),
      [err(top.location, "Eta-expansion needed? ")])
  | [] -> []
  end;
  top.synTys = exprsNil(location=builtin());

  top.substExprs = \s::Subst -> exprsNil(location=top.location);
}

inherited attribute inhTy :: Maybe<Expr> occurs on Expr;
synthesized attribute argInhTys :: [Maybe<Expr>] occurs on Expr;
synthesized attribute substExpr :: (Expr ::= Subst) occurs on Expr;
synthesized attribute synTy :: Expr occurs on Expr;

aspect default production
top::Expr ::=
{
  top.argInhTys = [];
  top.synTy = unificationVar(genInt(), location=builtin());
}

aspect production call
top::Expr ::= f::String xs::Exprs
{
  xs.inhTys = case lookupTyEnv(f, top.inhTyEnv) of
  | just(t) -> t.argInhTys
  | _ -> []
  end;

  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty)]
  | nothing() -> []
  end;

  top.constraints <- xs.constraints;
  top.hasVars = xs.hasVars;
  top.substExpr = \s::Subst -> call(f, xs.substExprs(s), location=top.location);
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  body.inhTy = case top.inhTy of
  | just(pi(nothing(), _, t)) -> just(t)
  | nothing() -> nothing()
  end;
  body.inhTyEnv = let
    ty :: Maybe<Expr> =
      case top.inhTy of
      | just(pi(just(xn), xt, t)) -> error("TODO lam inhTy")
      | just(pi(nothing(), _, t)) -> just(t)
      | nothing() -> nothing()
      end
  in
    pair(name, ty) :: top.inhTyEnv
  end;

  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty)]
  | nothing() -> []
  end;

  top.constraints <- body.constraints;
  top.hasVars = body.hasVars;
  top.substExpr = \s::Subst -> lam(name, body.substExpr(s), location=top.location);
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  l.inhTy = nothing();
  r.inhTy = nothing();
  r.inhTyEnv = error("TODO pi inhTyEnv");

  top.argInhTys = just(l) :: r.argInhTys;
  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty)]
  | nothing() -> []
  end;

  top.constraints <- l.constraints ++ r.constraints;
  top.hasVars = l.hasVars || r.hasVars;
  top.substExpr = \s::Subst -> pi(name, l.substExpr(s), r.substExpr(s), location=top.location);
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.constraints := [];
  top.hasVars = true;
  top.substExpr = \s::Subst -> case s of 
    | substVar(id2, e) -> if id == id2 then e else top
    end;
  top.synTy = case top.inhTy of
  | just(ty) -> ty
  | nothing() -> error("Cannot infer type of ?" ++ toString(id))
  end;
}
