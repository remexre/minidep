grammar edu:umn:cs:melt:minidep:abstractsyntax:spined;

import edu:umn:cs:melt:minidep:concretesyntax only pp;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

nonterminal Subst;

closed nonterminal Constraint with pp, solve;
synthesized attribute solve :: Pair<[Constraint] [Subst]>;

abstract production constraintEq
top::Constraint ::= l::Expr r::Expr
{
  top.pp = ppConcat(
    [ l.expr5_c.pp
    , text(" ~ ")
    , r.expr5_c.pp
    ]);
  top.solve = error("TODO Solve " ++ show(80, top.pp));
}

autocopy attribute inhTyEnv :: [Pair<String Expr>] occurs on Decl, Decls, Expr, Exprs, Implicits;
synthesized attribute constraints :: [Constraint] with ++;
synthesized attribute hasVars :: Boolean occurs on Decl, Decls, Expr, Exprs;

synthesized attribute substDecls :: (Decls ::= Subst) occurs on Decls;
synthesized attribute unified :: Decls occurs on Decls;

function solveAll
Decls ::= cs::[Constraint] ds::Decls
{
  return if !null(cs) then
    let
      soln :: Pair<[Constraint] [Subst]> = head(cs).solve
    in
      solveAll(soln.fst ++ tail(cs), substAll(soln.snd, ds))
    end
  else ds;
}

function substAll
Decls ::= ss::[Subst] ds::Decls
{
  return if !null(ss) then
    substAll(tail(ss), ds.substDecls(head(ss)))
  else ds;
}

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.constraints := h.constraints ++ t.constraints;
  top.hasVars = h.hasVars || t.hasVars;
  top.unified = solveAll(top.constraints, top);

  top.substDecls = \s::Subst -> error("TODO substDecls");
}

aspect production declsNil
top::Decls ::=
{
  top.constraints := [];
  top.hasVars = false;
  top.unified = declsNil();

  top.substDecls = \s::Subst -> top;
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
  ty.inhTyEnv = implicits.asList;
  body.inhTyEnv = implicits.asList;

  top.constraints := implicits.constraints ++ ty.constraints ++ body.constraints;
  top.hasVars = implicitsHaveVars(implicits) || ty.hasVars || body.hasVars;

  top.substDecl = \s::Subst -> error("TODO substDecl");
}

synthesized attribute substImplicits :: (Implicits ::= Subst) occurs on Implicits;

aspect production implicitsCons
top::Implicits ::= name::String ex::Expr tl::Implicits
{
  ex.inhTy = nothing();
  top.constraints := ex.constraints ++ tl.constraints;

  top.substImplicits = \s::Subst -> error("TODO substImplicits implicitsCons");
}

aspect production implicitsNil
top::Implicits ::=
{
  top.constraints := [];

  top.substImplicits = \s::Subst -> error("TODO substImplicits implicitsNil");
}

inherited attribute inhTys :: [Maybe<Expr>] occurs on Exprs;
synthesized attribute substExprs :: (Exprs ::= Subst) occurs on Exprs;
synthesized attribute synTys :: Exprs occurs on Exprs;

aspect production exprsCons
top::Exprs ::= hd::Expr tl::Exprs
{
  top.constraints := hd.constraints ++ tl.constraints;
  top.hasVars = hd.hasVars || tl.hasVars;

  top.errors <- case top.inhTys of
  | _::_ -> []
  | [] -> [err(top.location, "Too many arguments provided?")]
  end;
  hd.inhTy = head(top.inhTys);
  tl.inhTys = case top.inhTys of
  | _::t -> t
  | [] -> []
  end;
  top.synTys = exprsCons(hd.synTy, tl.synTys, location=builtin());

  top.substExprs = \s::Subst -> error("TODO substExprs exprsCons");
}

aspect production exprsNil
top::Exprs ::=
{
  top.constraints := [];
  top.hasVars = false;

  top.errors <- case top.inhTys of
  | _::_ -> [err(top.location, "Eta-expansion needed?")]
  | [] -> []
  end;
  top.synTys = exprsNil(location=builtin());

  top.substExprs = \s::Subst -> error("TODO substExprs exprsNil");
}

inherited attribute inhTy :: Maybe<Expr> occurs on Expr;
synthesized attribute substExpr :: (Expr ::= Subst) occurs on Expr;
synthesized attribute synTy :: Expr occurs on Expr;

aspect default production
top::Expr ::=
{
  top.synTy = error("TODO synTy");
  top.substExpr = \s::Subst -> error("TODO substExpr");
}

aspect production call
top::Expr ::= f::String xs::Exprs
{
  xs.inhTys = case lookupBy(stringEq, f, top.inhTyEnv) of
  | just(t) -> error("TODO call inhTys just")
  | nothing() -> []
  end;

  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty)]
  | nothing() -> []
  end;

  top.constraints <- xs.constraints;
  top.hasVars = xs.hasVars;
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  body.inhTy = case top.inhTy of
  | just(pi(nothing(), _, t)) -> just(t)
  | nothing() -> nothing()
  end;
  body.inhTyEnv = error("TODO lam inhTyEnv");

  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty)]
  | nothing() -> []
  end;

  top.constraints <- body.constraints;
  top.hasVars = body.hasVars;
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  l.inhTy = nothing();
  r.inhTy = nothing();
  r.inhTyEnv = error("TODO pi inhTyEnv");

  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty)]
  | nothing() -> []
  end;

  top.constraints <- l.constraints ++ r.constraints;
  top.hasVars = l.hasVars || r.hasVars;
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.constraints := [];
  top.hasVars = true;
  top.synTy = case top.inhTy of
  | just(ty) -> ty
  | nothing() -> error("Cannot infer type of ?" ++ toString(id))
  end;
}
