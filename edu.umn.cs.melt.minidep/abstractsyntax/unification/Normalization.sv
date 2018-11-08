grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

import edu:umn:cs:melt:minidep:util;

autocopy attribute inhValEnv :: [Pair<String Maybe<Expr>>] occurs on Decl, Decls, Expr;
synthesized attribute normal :: Boolean occurs on Expr;
synthesized attribute normalized :: Expr occurs on Expr;
synthesized attribute reduce :: Expr occurs on Expr;

aspect default production
top::Expr ::=
{
  top.normalized = if top.normal then top else top.reduce.normalized;
}

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.normal = x.normal && case f of
  | lam(_, _) -> false
  | _ -> f.normal
  end;
  top.reduce = if x.normal then
    if f.normal then
      case f of
      | lam(n, b) -> b.beta(n, x)
      | _ -> top
      end
    else
      app(f.reduce, x, location=top.location)
  else
    app(f, x.reduce, location=top.location);
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  top.normal = true;
  top.reduce = top;
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.normal = true;
  top.reduce = top;
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.normal = true;
  top.reduce = top;
}

aspect production universe
top::Expr ::=
{
  top.normal = true;
  top.reduce = top;
}

aspect production var
top::Expr ::= s::String
{
  top.normal = !lookupEnv(s, top.inhValEnv).isJust;
  top.reduce = case lookupEnv(s, top.inhValEnv) of
  | just(x) -> x
  | nothing() -> top
  end;
}
