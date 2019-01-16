grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

import edu:umn:cs:melt:minidep:util;

autocopy attribute inhValEnv :: [Pair<String Maybe<Expr>>] occurs on Decl, Decls, Expr;
synthesized attribute normal :: Boolean occurs on Expr;
synthesized attribute normalized :: Expr occurs on Expr;

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  t.inhValEnv = h.synValEnv ++ top.inhValEnv;
}

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.normal = x.normal && case f of
  | lam(_, _) -> false
  | _ -> f.normal
  end;

  local reduced :: Expr = if x.normal then
    if f.normal then
      case f of
      | lam(n, b) -> b.beta(n, x)
      | _ -> top
      end
    else
      app(f.normalized, x, location=top.location)
  else
    app(f, x.normalized, location=top.location);
  reduced.inhValEnv = top.inhValEnv;
  top.normalized = if reduced.normal then reduced else reduced.normalized;
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  top.normal = body.normal;
  local reduced :: Expr = lam(name, body.normalized, location=top.location);
  reduced.inhValEnv = top.inhValEnv;
  top.normalized = reduced;
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.normal = true;
  top.normalized = top;
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.normal = true;
  top.normalized = top;
}

aspect production universe
top::Expr ::=
{
  top.normal = true;
  top.normalized = top;
}

aspect production var
top::Expr ::= s::String
{
  top.normal = !lookupEnv(s, top.inhValEnv).isJust;
  top.normalized = case lookupEnv(s, top.inhValEnv) of
  | just(x) -> x
  | nothing() -> top
  end;
}
