grammar edu:umn:cs:melt:minidep:abstractsyntax:spined;

import edu:umn:cs:melt:minidep:concretesyntax only pp;
import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treemap as rtm;
import silver:util:raw:treemap only Map;

-- TODO: Probably remove more closed :P
nonterminal Subst;

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

function implicitsConstraints
[Constraint] ::= implicits::Map<String Expr>
{
  return flatMap(\p::Pair<String Expr> -> p.snd.constraints,
    rtm:toList(implicits));
}

function implicitsHaveVars
Boolean ::= implicits::Map<String Expr>
{
  return foldr(\p::Pair<String Expr> has::Boolean -> has || p.snd.hasVars,
    false, rtm:toList(implicits));
}

synthesized attribute substSignature :: (Signature ::= Subst) occurs on Signature;

aspect production sig
top::Signature ::= implicits::Map<String Expr> ty::Expr
{
  top.constraints = implicitsConstraints(implicits) ++ ty.constraints;
  top.hasVars = implicitsHaveVars(implicits) || ty.hasVars;
}

synthesized attribute substDecl :: (Decl ::= Subst) occurs on Decl;

aspect production decl
top::Decl ::= name::String implicits::Map<String Expr> ty::Expr body::Expr
{
  top.constraints = implicitsConstraints(implicits) ++ ty.constraints ++ body.constraints ++
    [ constraintHasTy(body, ty)
    ];
  top.hasVars = implicitsHaveVars(implicits) || ty.hasVars || body.hasVars;
}

synthesized attribute substExpr :: (Expr ::= Subst) occurs on Expr;

aspect production call
top::Expr ::= f::Expr xs::[Expr]
{
  top.constraints = f.constraints ++ flatMap((.constraints), xs);
  top.hasVars = foldr(\ex::Expr has::Boolean -> has || ex.hasVars, f.hasVars, xs);
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
top::Expr ::= name::String implicits::Map<String Expr>
{
  top.constraints = implicitsConstraints(implicits);
  top.hasVars = implicitsHaveVars(implicits);
}
