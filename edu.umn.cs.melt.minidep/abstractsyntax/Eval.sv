grammar edu:umn:cs:melt:minidep:abstractsyntax;

-- import edu:umn:cs:melt:minidep:concretesyntax;
import edu:umn:cs:melt:minidep:util;
-- import silver:langutil;
-- import silver:langutil:pp;

synthesized attribute isNeutral :: Boolean occurs on Expr;
synthesized attribute normalized :: Expr occurs on Root, Expr;

aspect production root
top::Root ::= e::Expr
{
  top.normalized = e.normalized;
}

-- Operator productions.

aspect production lam
top::Expr ::= arg::String body::Expr
{
  top.isNeutral = true;
  top.normalized = lam(arg, body.normalized, location=top.location);
}

aspect production pi
top::Expr ::= arg::Maybe<String> ty::Expr body::Expr
{
  top.isNeutral = true;
  top.normalized = pi(arg, ty.normalized, body.normalized, location=top.location);
}

aspect production tyAnnot
top::Expr ::= l::Expr r::Expr
{
  top.isNeutral = l.isNeutral;
  top.normalized = l.normalized;
}

aspect production add
top::Expr ::= l::Expr r::Expr
{
  top.isNeutral = l.isNeutral || r.isNeutral;
  top.normalized = if top.isNeutral
    then add(l.normalized, r.normalized, location=top.location)
    else case l.normalized, r.normalized of
    | nat(l), nat(r) -> nat(l + r, location=builtin())
    | l, r -> error("type error in add: " ++ ppsExpr(l) ++ ", " ++ ppsExpr(r))
    end;
}

aspect production mul
top::Expr ::= l::Expr r::Expr
{
  top.isNeutral = l.isNeutral || r.isNeutral;
  top.normalized = if top.isNeutral
    then mul(l.normalized, r.normalized, location=top.location)
    else case l.normalized, r.normalized of
    | nat(l), nat(r) -> nat(l * r, location=builtin())
    | l, r -> error("type error in mul: " ++ ppsExpr(l) ++ ", " ++ ppsExpr(r))
    end;
}

aspect production app
top::Expr ::= l::Expr r::Expr
{
  top.isNeutral = l.isNeutral || top.normalized.isNeutral;
  top.normalized = if l.isNeutral
    then add(l.normalized, r.normalized, location=top.location)
    else case l.normalized of
    | lam(arg, body) -> decorate body with {
        env = cons(pair(arg, pair(r, r.synTy)), top.env);
        inhTy = top.inhTy;
      }.normalized
    | e -> error("type error in app: " ++ ppsExpr(e))
    end;
}

-- Literal and Identifier productions.

aspect production var
top::Expr ::= name::Maybe<String>
{
  top.isNeutral = case name of
  | just(name) -> isNothing(lookupBy(stringEq, name, top.env))
  | nothing() -> true
  end;
  top.normalized = case name of
  | just(name) -> case lookupBy(stringEq, name, top.env) of
    | just(pair(val, _)) -> val
    | nothing() -> top
    end
  | nothing() -> top
  end;
}

aspect production nat
top::Expr ::= n::Integer
{
  top.isNeutral = false;
  top.normalized = top;
}

aspect production natTy
top::Expr ::=
{
  top.isNeutral = false;
  top.normalized = top;
}

aspect production typeKind
top::Expr ::=
{
  top.isNeutral = false;
  top.normalized = top;
}
