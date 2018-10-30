grammar edu:umn:cs:melt:minidep:abstractsyntax:spined;

import silver:util:raw:treemap as rtm;
import silver:util:raw:treemap only Map;

synthesized attribute etaExpandDecls :: Decls occurs on Decls;

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
}

aspect production declsNil
top::Decls ::=
{
}

synthesized attribute etaExpandSignature :: Signature occurs on Signature;

aspect production sig
top::Signature ::= implicits::Map<String Expr> ty::Expr
{
}

synthesized attribute etaExpandDecl :: Decl occurs on Decl;

aspect production decl
top::Decl ::= name::String implicits::Map<String Expr> ty::Expr body::Expr
{
}

synthesized attribute etaExpandExpr :: Expr occurs on Expr;
synthesized attribute piCount :: Integer occurs on Expr;

aspect default production
top::Expr ::=
{ top.piCount = 0; }

aspect production call
top::Expr ::= f::Expr xs::[Expr]
{
}

aspect production lam
top::Expr ::= name::String body::Expr
{
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.piCount = r.piCount + 1;
}

aspect production unificationVar
top::Expr ::= id::Integer
{
}

aspect production var
top::Expr ::= name::String implicits::Map<String Expr>
{
}
