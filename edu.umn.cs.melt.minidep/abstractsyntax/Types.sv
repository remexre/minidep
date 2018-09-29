grammar edu:umn:cs:melt:minidep:abstractsyntax;

-- import edu:umn:cs:melt:minidep:concretesyntax;
-- import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

function typeError
[Message] ::= ex::Expr got::Expr exp::Expr
{
  return typeErrorAny(ex, got, cat(text("the type "), ppExpr(exp)));
}

function typeErrorAny
[Message] ::= ex::Expr got::Expr exp::Document
{
  local msg :: Document = cat(cat(cat(text("The expression "), ppExpr(ex)), text(" has type ")),
      cat(cat(ppExpr(got), text(", but ")), cat(exp, text(" was expected."))));
  return [err(ex.location, msg.result)];
}

inherited attribute inhTy :: Maybe<Expr> occurs on Expr;
synthesized attribute synTy :: Expr occurs on Expr;

aspect production root
top::Root ::= e::Expr
{
  e.inhTy = nothing();
}

-- Operator productions.

aspect production lam
top::Expr ::= arg::String body::Expr
{
  local inhTy :: Maybe<Expr> = mapMaybe((.normalized), top.inhTy);
  top.errors <- case inhTy of
  | just(pi(_, _, _)) -> []
  | just(ty) -> typeErrorAny(top, ty, text("a pi type"))
  | nothing() -> [err(top.location, "Cannot infer type to " ++ ppsExpr(top))]
  end;

  body.inhTy = case inhTy of
  | just(pi(_, _, b)) -> just(b)
  | _ -> error("lam's type cannot be inferred")
  end;
  top.synTy = fromMaybe(error("lam's type cannot be inferred"), inhTy);
}

{-
aspect production pi
top::Expr ::= arg::Maybe<String> ty::Expr body::Expr
{
  {-
  body.env = case top.inhTy of
  | just(inhTy) -> top.env -- TODO
  -- | just(t) -> error("type error in lam inhTy: " ++ ppExpr(t).result)
  | nothing() -> error("type error in lam inhTy")
  end;
  body.inhTy = nothing();

  top.errors := body.errors;
  -- TODO
  top.synTy = error("TODO");
  -}
}
-}

aspect production tyAnnot
top::Expr ::= l::Expr r::Expr
{
  top.errors <- if exprEq(l.synTy.normalized, top.synTy)
    then []
    else typeError(l, l.synTy.normalized, top.synTy);

  l.inhTy = just(top.synTy);
  r.inhTy = nothing();
  top.synTy = r.normalized;
}

{-
aspect production add
top::Expr ::= l::Expr r::Expr
{
  l.env = top.env;
  r.env = top.env;
  -- l/r's inhTy comes from checkType.

  top.errors := l.errors ++ r.errors;
  top.errors <- checkType(l, natTy(location=builtin()));
  top.errors <- checkType(r, natTy(location=builtin()));
  top.synTy = natTy(location=builtin());
}

aspect production mul
top::Expr ::= l::Expr r::Expr
{
  l.env = top.env;
  r.env = top.env;
  -- l/r's inhTy comes from checkType.

  top.errors := l.errors ++ r.errors;
  top.errors <- checkType(l, natTy(location=builtin()));
  top.errors <- checkType(r, natTy(location=builtin()));
  top.synTy = natTy(location=builtin());
}

aspect production app
top::Expr ::= l::Expr r::Expr
{
  l.env = top.env;
  r.env = top.env;
  l.inhTy = nothing();
  r.inhTy = nothing();

  top.errors := l.errors ++ r.errors;
  -- TODO
  top.synTy = r.normalized;
}

-- Literal and Identifier productions.

aspect production var
top::Expr ::= name::Maybe<String>
{
  local tyM :: Maybe<Expr> = mapMaybe(snd, lookupBy(stringEq, name, top.env));
  local ty :: Expr = fromMaybe(error("undefined var: " ++ name), tyM);
  top.errors := case tyM of
  | just(_) -> []
  | nothing() -> [err(top.location, "No binding for " ++ name)]
  end;
  top.synTy = ty;
}

aspect production nat
top::Expr ::= n::Integer -- RIP no unsigned ints
{
  top.errors := if n < 0
    then [err(top.location, "Somehow a negative number got in here?")]
    else [];
  top.synTy = natTy(location=builtin());
}

aspect production natTy
top::Expr ::=
{
  top.errors := [];
  top.synTy = typeKind(location=builtin());
}

aspect production typeKind
top::Expr ::=
{
  top.errors := [];
  top.synTy = error("Universe Polymorphism is hard");
}
-}
