grammar edu:umn:cs:melt:minidep:abstractsyntax:spined;

import edu:umn:cs:melt:minidep:concretesyntax only pi_c, pp;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treemap as rtm;
import silver:util:raw:treemap only Map;
import silver:util:raw:treeset as set;

closed nonterminal Subst;

nonterminal Constraint with pp, solve;
synthesized attribute solve :: Pair<[Constraint] [Subst]>;

abstract production constraintEq
top::Constraint ::= l::Decorated Expr r::Decorated Expr
{
  top.pp = ppConcat(
    [ l.expr5_c.pp
    , text(" ?= ")
    , r.expr5_c.pp
    ]);
  top.solve = error("TODO Solve " ++ show(80, top.pp));
}

abstract production constraintHasTy
top::Constraint ::= l::Decorated Expr r::Decorated Expr
{
  top.pp = ppConcat(
    [ l.expr5_c.pp
    , text(" ?: ")
    , r.expr5_c.pp
    ]);
  top.solve = case l, r of
  | call(f, xs), r -> error("Not a pi type: " ++ show(80, r.expr1_c.pp))
  | var(name, imps), _ ->
      case lookupBy(stringEq, name, error("env")) of
      | just(just(sig(imps, ty))) -> error("TODO jj in hasTy")
      | just(nothing()) -> error("TODO jn in hasTy")
      | nothing() -> error("TODO n in hasTy")
      end
  | _, _ -> error("TODO Solve " ++ show(80, top.pp))
  end;
}

abstract production constraintIsPiArg
top::Constraint ::= f::Decorated Expr x::Decorated Expr
{
  top.pp = ppConcat(
    [ f.expr5_c.pp
    , text(" ?= Pi _:")
    , x.expr2_c.pp
    , text(". _")
    ]);
  top.solve = error("TODO Solve " ++ show(80, top.pp));
}

abstract production constraintIsPiRet
top::Constraint ::= f::Decorated Expr y::Decorated Expr
{
  top.pp = ppConcat(
    [ f.expr5_c.pp
    , text(" ?= Pi _:_. ")
    , y.expr1_c.pp
    ]);
  top.solve = error("TODO Solve " ++ show(80, top.pp));
}

synthesized attribute constraints :: [Constraint] occurs on Decl, Decls, Expr, Signature;
synthesized attribute hasVars :: Boolean occurs on Decl, Decls, Expr, Signature;

synthesized attribute substDecls :: (Decls ::= Subst) occurs on Decls;
synthesized attribute unified :: Decls occurs on Decls;

{-
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
  top.constraints = h.constraints ++ t.constraints;
  top.hasVars = h.hasVars || t.hasVars;
  top.unified = solveAll(top.constraints, top);
}

aspect production declsNil
top::Decls ::=
{
  top.constraints = [];
  top.hasVars = false;
  top.unified = declsNil();
}

synthesized attribute substSignature :: (Signature ::= Subst) occurs on Signature;

aspect production sig
top::Signature ::= implicits::Implicits ty::Expr
{
  top.constraints = implicits.constraints ++ ty.constraints;
  top.hasVars = implicits.hasVars || ty.hasVars;
}

synthesized attribute substDecl :: (Decl ::= Subst) occurs on Decl;

aspect production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  top.constraints = implicits.constraints ++ ty.constraints ++ body.constraints ++
    [ constraintHasTy(body, ty)
    ];
  top.hasVars = implicits.hasVars || ty.hasVars || body.hasVars;
}

synthesized attribute substImplicits :: (Implicits ::= Subst) occurs on Implicits;

aspect production implicitsCons
top::Implicits ::= n::String e::Expr t::Implicits
{
  top.constraints = e.constraints ++ t.constraints;
  top.hasVars = e.hasVars || t.hasVars;
}

aspect production implicitsNil
top::Implicits ::=
{
  top.constraints = [];
  top.hasVars = false;
}

synthesized attribute substExpr :: (Expr ::= Subst) occurs on Expr;

aspect production app
top::Expr ::= l::Expr r::Expr
{
  local argTy :: Expr = unificationVar(genInt(), location=builtin());
  top.constraints = l.constraints ++ r.constraints ++
    [ -- constraintIsPiArg(l, argTy)
    -- , constraintHasTy(r, argTy)
    ];
  top.hasVars = l.hasVars || r.hasVars;
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  top.constraints = body.constraints;
  top.hasVars = body.hasVars;
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.constraints = l.constraints ++ r.constraints;
  top.hasVars = l.hasVars || r.hasVars;
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.constraints = [];
  top.hasVars = true;
}

aspect production var
top::Expr ::= name::String implicits::Implicits
{
  top.constraints = [];
  top.hasVars = implicits.hasVars;
}
-}
