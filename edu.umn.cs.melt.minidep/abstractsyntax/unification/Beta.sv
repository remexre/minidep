grammar edu:umn:cs:melt:minidep:abstractsyntax:unification;

synthesized attribute beta :: (Expr ::= String Expr) occurs on Expr;

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.beta = \n::String e::Expr -> app(f.beta(n, e), x.beta(n, e), location=top.location);
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  top.beta = \n::String e::Expr -> if name == n then
      top
    else
      lam(name, body.beta(n, e), location=top.location);
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.beta = \n::String e::Expr -> case name of
  | just(n2) -> if n == n2
                then pi(just(n2), l.beta(n, e), r, location=top.location)
                else pi(just(n2), l.beta(n, e), r.beta(n, e), location=top.location)
  | nothing() -> pi(nothing(), l.beta(n, e), r.beta(n, e), location=top.location)
  end;
}

aspect production unificationVar
top::Expr ::= id::Integer
{
  top.beta = \n::String e::Expr -> top;
}

aspect production universe
top::Expr ::=
{
  top.beta = \n::String e::Expr -> top;
}

aspect production var
top::Expr ::= s::String
{
  top.beta = \n::String e::Expr -> if s == n then e else top;
}
