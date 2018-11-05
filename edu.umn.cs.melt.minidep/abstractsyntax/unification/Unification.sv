grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

import edu:umn:cs:melt:minidep:concretesyntax only pp;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

nonterminal Subst;
synthesized attribute substSubst :: (Subst ::= Subst) occurs on Subst;
synthesized attribute substSubsts :: ([Subst] ::= [Subst]) occurs on Subst;

abstract production substVar
top::Subst ::= var::Integer expr::Expr
{
  top.substSubst = \s::Subst -> substVar(var, expr.substExpr(s));
  top.substSubsts = map(\s::Subst -> s.substSubst(top), _);
}

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
  top.solve = if l.eq(r)
    then pair([], nothing())
    else case l, r of
    | unificationVar(l), _ -> pair([], just(substVar(l, r)))
    | _, unificationVar(_) -> pair([constraintEq(r, l)], nothing())
    | app(lf, lx), app(rf, rx) -> pair([constraintEq(lf, rf), constraintEq(lx, rx)], nothing())
    | _, _ -> error("TODO Solve " ++ show(80, top.pp))
    end;
}

autocopy attribute inhTyEnv :: [Pair<String Maybe<Expr>>] occurs on Decl, Decls, Expr;
synthesized attribute constraints :: [Constraint] with ++;
attribute constraints occurs on Decl, Decls, Expr;
synthesized attribute hasVars :: Boolean occurs on Decl, Decls, Expr;

synthesized attribute substDecls :: (Decls ::= Subst) occurs on Decls;
synthesized attribute substsDecls :: (Decls ::= [Subst]) occurs on Decls;
synthesized attribute unified :: Decls occurs on Decls;

function solveAll
[Subst] ::= cs::[Constraint] ss::[Subst]
{
  return case cs of
  | h::t -> case h.solve.snd of
            | just(s) ->   solveAll(h.solve.fst ++ t, s::ss)
            | nothing() -> solveAll(h.solve.fst ++ t, ss)
            end
  | [] -> ss
  end;
}

aspect default production
top::Decls ::=
{
  top.substsDecls = \ss::[Subst] -> case ss of
  | h::t -> top.substDecls(h).substsDecls(h.substSubsts(t))
  | [] -> top
  end;
  top.unified = top.substsDecls(solveAll(top.constraints, []));
}

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.constraints := h.constraints ++ t.constraints;
  top.hasVars = h.hasVars || t.hasVars;
  top.substDecls = \s::Subst -> declsCons(h.substDecl(s), t.substDecls(s));
}

aspect production declsNil
top::Decls ::=
{
  top.constraints := [];
  top.hasVars = false;
  top.substDecls = \s::Subst -> declsNil();
}

synthesized attribute substDecl :: (Decl ::= Subst) occurs on Decl;

aspect production decl
top::Decl ::= name::String ty::Expr body::Expr
{
  ty.inhTy = nothing();
  body.inhTy = just(ty);
  ty.inhTyEnv = top.inhTyEnv;
  body.inhTyEnv = top.inhTyEnv; -- TODO: Include pi type variables from ty?

  top.constraints := ty.constraints ++ body.constraints;
  top.hasVars = ty.hasVars || body.hasVars;
  top.substDecl = \s::Subst ->
    decl(name, ty.substExpr(s), body.substExpr(s), location=top.location);
}

inherited attribute inhTy :: Maybe<Expr> occurs on Expr;
synthesized attribute substExpr :: (Expr ::= Subst) occurs on Expr;
synthesized attribute synTy :: Expr occurs on Expr;

aspect default production
top::Expr ::=
{
  top.synTy = unificationVar(genInt(), location=top.location);
}

aspect production app
top::Expr ::= f::Expr x::Expr
{
  f.inhTy = nothing();
  f.inhTyEnv = top.inhTyEnv;
  x.inhTy = case f.synTy of
  | pi(_, t, _) -> just(t)
  | _ -> nothing()
  end;
  x.inhTyEnv = case f.synTy of
  | pi(just(n), l, _) -> [pair(n, just(l))]
  | _ -> []
  end ++ top.inhTyEnv;

  top.errors <- case f.synTy of
  | pi(_, t, _) -> []
  | t -> [wrn(top.location, show(80, cat(t.pp, text(" is not a pi type"))))]
  end;

  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty)]
  | nothing() -> []
  end;
  top.constraints <- f.constraints ++ x.constraints;
  top.hasVars = f.hasVars || x.hasVars;
  top.substExpr = \s::Subst -> app(f.substExpr(s), x.substExpr(s), location=top.location);
  top.synTy = case f.synTy of
  | pi(just(n), _, r) -> r.beta(n, x)
  | pi(nothing(), _, r) -> r
  | t -> error(show(80, cat(t.pp, text(" is not a pi type"))))
  end;
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  local argTy :: Maybe<Expr> =
    case top.inhTy of
    | just(pi(_, t, _)) -> just(t)
    | nothing() -> nothing()
    end;

  body.inhTy = case top.inhTy of
  | just(pi(_, _, t)) -> just(t)
  | nothing() -> nothing()
  end;
  body.inhTyEnv = let
    envTmp :: [Pair<String Maybe<Expr>>] = pair(name, argTy) :: top.inhTyEnv
  in case top.inhTy of
     | just(pi(just(n), t, _)) -> pair(n, just(t)) :: envTmp
     | _ -> envTmp
     end
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
  r.inhTyEnv = case name of
  | just(n) -> pair(n, just(l)) :: top.inhTyEnv
  | nothing() -> top.inhTyEnv
  end;

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
  top.errors <- case top.inhTy of
  | just(ty) -> []
  | nothing() -> [err(top.location, "Cannot infer type of ?" ++ toString(id))]
  end;
  top.hasVars = true;
  top.substExpr = \s::Subst -> case s of 
    | substVar(id2, e) -> if id == id2 then e else top
    end;
  top.synTy = case top.inhTy of
  | just(ty) -> ty
  | nothing() -> error("Cannot infer type of ?" ++ toString(id))
  end;
}

aspect production universe
top::Expr ::=
{
  top.constraints := [];
  top.errors <- case top.inhTy of
  | just(ty) -> [err(top.location, "TYPE has no type")]
  | nothing() -> []
  end;
  top.hasVars = false;
  top.substExpr = \s::Subst -> top;
  top.synTy = error("TYPE has no type");
}

aspect production var
top::Expr ::= s::String
{
  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty)]
  | nothing() -> []
  end;
  top.errors <- case lookupTyEnv(s, top.inhTyEnv) of
  | just(t) -> []
  | nothing() -> [err(top.location, "Cannot infer type of " ++ s)]
  end;
  top.hasVars = false;
  top.substExpr = \s::Subst -> top;
  top.synTy = case lookupTyEnv(s, top.inhTyEnv) of
  | just(t) -> t
  | nothing() -> error("Cannot infer type of " ++ s)
  end;
}
