grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

import edu:umn:cs:melt:minidep:concretesyntax only pp;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treeset as set;

nonterminal Subst;

abstract production substVar
top::Subst ::= var::Integer expr::Expr
{}

closed nonterminal Constraint with cause, location, pp, solve, substConstraint;
synthesized attribute cause :: Maybe<Constraint>;
synthesized attribute solve :: Either<[Message] Pair<[Constraint] [Subst]>>;
synthesized attribute substConstraint :: (Constraint ::= Subst);

aspect default production
top::Constraint ::=
{
  top.cause = nothing();
}

function causes
[Constraint] ::= c::Constraint
{
  return case c.cause of
  | just(c) -> c :: causes(c)
  | nothing() -> []
  end;
}

abstract production constraintEq
top::Constraint ::= l::Expr r::Expr cause::Maybe<Constraint>
{
  top.cause = cause;
  top.pp = ppConcat(
    [ l.expr5_c.pp
    , text(" ~ ")
    , r.expr5_c.pp
    ]);

  local decL :: Expr = l;
  local decR :: Expr = r;

  decL.inhTyEnv = error("TODO");
  decR.inhTyEnv = error("TODO");

  local solve :: Maybe<Pair<[Constraint] [Subst]>> =
    case l, r of
    | unificationVar(l), _ -> just(pair([], [substVar(l, r)]))
    | _, unificationVar(_) -> just(pair([constraintEq(r, l, just(top), location=top.location)], []))
    | app(lf, lx), app(rf, rx) -> just(pair([ constraintEq(lf, rf, just(top), location=top.location)
                                            , constraintEq(lx, rx, just(top), location=top.location)
                                            ], []))
    | pi(nothing(), ll, lr), pi(nothing(), rl, rr) -> just(pair([ constraintEq(ll, rl, just(top), location=top.location)
                                                                , constraintEq(lr, rr, just(top), location=top.location)
                                                                ], []))
    | pi(ln, ll, lr), pi(rn, rl, rr) ->
        let
          newName :: String = genSym()
        in let
             lpi2 :: Expr = case ln of
             | just(n) -> lr.beta(n, var(newName, location=l.location))
             | nothing() -> lr
             end,
             rpi2 :: Expr = case rn of
             | just(n) -> rr.beta(n, var(newName, location=r.location))
             | nothing() -> rr
             end
           in
             just(pair([ constraintEq(ll, rl, just(top), location=top.location)
                       , constraintEq(lpi2, rpi2, just(top), location=top.location)
                       ], []))
           end
        end
    | universe(), universe() -> just(pair([], []))
    | var(ln), var(rn) -> if ln == rn
                        then just(pair([], []))
                        else nothing()
    | _, _ -> nothing()
    end;
  top.solve = case solve of
  | just(x) -> right(x)
  | nothing() ->
      let
        baseMsg :: Document = ppImplode(space(),
          [ text("Cannot unify")
          , l.expr5_c.pp
          , text("with")
          , r.expr5_c.pp
          ]),
        causeMsgs :: [Document] = map(\c::Constraint -> cat(text("While solving "), c.pp), causes(top))
      in
        left([err(top.location, show(80, ppImplode(line(), causeMsgs ++ [baseMsg])))])
      end
  end;
  top.substConstraint = \s::Subst -> constraintEq(l.substExpr(s), r.substExpr(s), cause, location=top.location);
}

synthesized attribute unificationVars :: set:Set<Pair<Integer Location>> occurs on Decl, Decls, Expr, Root;
synthesized attribute unified :: Root occurs on Root;

autocopy attribute inhTyEnv :: [Pair<String Maybe<Expr>>] occurs on Decl, Decls, Expr;
synthesized attribute constraints :: [Constraint] with ++;
attribute constraints occurs on Decl, Expr;

synthesized attribute substDecls :: (Decls ::= Subst) occurs on Decls;
synthesized attribute substsDecls :: (Decls ::= [Subst]) occurs on Decls;
synthesized attribute unifiedDecls :: Decls occurs on Decls;

function solveAll
Either<[Message] [Subst]> ::= cs::[Constraint] ss::[Subst]
{
  return case cs of
  | h::t ->
      case h.solve of
      | left(errs) -> left(errs)
      | right(pair(newCs, newSs)) -> solveAll(newCs ++ map(\c::Constraint ->
          foldr(\s::Subst c::Constraint -> c.substConstraint(s), c, newSs), t), newSs ++ ss)
      end
  | [] -> right(ss)
  end;
}

aspect default production
top::Decls ::=
{
  top.substsDecls = \ss::[Subst] -> case ss of
  | h::t -> top.substDecls(h).substsDecls(t)
  | [] -> top
  end;
}

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  t.inhTyEnv = h.sigs ++ top.inhTyEnv;

  top.unificationVars = set:union(h.unificationVars, t.unificationVars);
  top.substDecls = \s::Subst -> declsCons(h.substDecl(s), t.substDecls(s));

  local soln :: Either<[Message] [Subst]> = solveAll(h.constraints, []);
  top.errors <- fromLeft(soln, []);
  top.unifiedDecls = declsCons(h.substsDecl(soln.fromRight), decorate t.substsDecls(soln.fromRight) with {
    inhTyEnv = top.inhTyEnv;
  }.unifiedDecls);
}

aspect production declsNil
top::Decls ::=
{
  top.unificationVars = set:empty(compareFstInteger);
  top.substDecls = \s::Subst -> declsNil();
  top.unifiedDecls = top;
}

synthesized attribute sigs :: [Pair<String Maybe<Expr>>] occurs on Decl;
synthesized attribute substDecl :: (Decl ::= Subst) occurs on Decl;
synthesized attribute substsDecl :: (Decl ::= [Subst]) occurs on Decl;

aspect default production
top::Decl ::=
{
  top.substsDecl = \ss::[Subst] -> case ss of
  | h::t -> top.substDecl(h).substsDecl(t)
  | [] -> top
  end;
}

aspect production declDecl
top::Decl ::= name::String ty::Expr
{
  ty.inhTy = nothing();

  top.constraints := ty.constraints;
  top.unificationVars = ty.unificationVars;
  top.sigs = [pair(name, just(ty))];
  top.substDecl = \s::Subst -> declDecl(name, ty.substExpr(s), location=top.location);
}

aspect production declDef
top::Decl ::= name::String ty::Expr body::Expr
{
  ty.inhTy = nothing();
  body.inhTy = just(ty);

  top.constraints := ty.constraints ++ body.constraints;
  top.unificationVars = set:union(ty.unificationVars, body.unificationVars);
  top.sigs = [pair(name, just(ty))];
  top.substDecl = \s::Subst ->
    declDef(name, ty.substExpr(s), body.substExpr(s), location=top.location);
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
  x.inhTy = case f.synTy of
  | pi(_, t, _) -> just(t)
  | _ -> nothing()
  end;

  top.errors <- case f.synTy of
  | pi(_, t, _) -> []
  | t -> [wrn(top.location, show(80, cat(t.pp, text(" is not a pi type"))))]
  end;

  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty, nothing(), location=top.location)]
  | nothing() -> []
  end;
  top.constraints <- f.constraints ++ x.constraints;
  top.unificationVars = set:union(f.unificationVars, x.unificationVars);
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
  | just(ty) -> [constraintEq(top.synTy, ty, nothing(), location=top.location)]
  | nothing() -> []
  end;

  top.constraints <- body.constraints;
  top.unificationVars = body.unificationVars;
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
  | just(ty) -> [constraintEq(top.synTy, ty, nothing(), location=top.location)]
  | nothing() -> []
  end;

  top.constraints <- l.constraints ++ r.constraints;
  top.unificationVars = set:union(l.unificationVars, r.unificationVars);
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
  top.unificationVars = set:add([pair(id, top.location)], set:empty(compareFstInteger));
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
  top.unificationVars = set:empty(compareFstInteger);
  top.substExpr = \s::Subst -> top;
  top.synTy = error("TYPE has no type");
}

aspect production var
top::Expr ::= s::String
{
  top.constraints := case top.inhTy of
  | just(ty) -> [constraintEq(top.synTy, ty, nothing(), location=top.location)]
  | nothing() -> []
  end;
  top.errors <- case lookupBy(stringEq, s, top.inhTyEnv) of
  | just(just(t)) -> []
  | just(nothing()) -> [err(top.location, "Cannot infer type of bound variable " ++ s)]
  | nothing() -> [err(top.location, "Cannot infer type of free variable " ++ s)]
  end;
  top.unificationVars = set:empty(compareFstInteger);
  top.substExpr = \s::Subst -> top;
  top.synTy = case lookupBy(stringEq, s, top.inhTyEnv) of
  | just(just(t)) -> t
  | just(nothing()) -> error("Cannot infer type of bound variable " ++ s)
  | nothing() -> error("Cannot infer type of free variable " ++ s)
  end;
}
