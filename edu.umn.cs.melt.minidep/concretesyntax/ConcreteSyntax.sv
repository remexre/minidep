grammar edu:umn:cs:melt:minidep:concretesyntax;

import edu:umn:cs:melt:minidep:abstractsyntax;
import silver:langutil;
import silver:langutil:pp;

synthesized attribute ast<a> :: a;

-- The root nonterminal and associated productions.

nonterminal Root_c with ast<Root>, location, pp;

concrete production root_c
top::Root_c ::= expr::Expr1_c
{
  top.ast = root(expr.ast, location=top.location);
  top.pp = expr.pp;
}

{-
concrete production root_c
top::Root_c ::= decls::Decls_c
{
  top.ast = root(decls.ast, location=top.location);
}
-}

-- The declaration, claim, and definition nonterminals and productions.

{-
nonterminal Decl_c with ast<Decl>, location;
nonterminal Decls_c with ast<[Decl]>, location;
nonterminal Claim_c with location;
nonterminal Defs_c with ast<[Def]>, location;
nonterminal Def_c with ast<Def>, location;

concrete production decl_c
top::Decl_c ::= claim::Claim_c defs::Defs_c
{
  -- top.ast = TODO;
}
-}

-- The expression nonterminals.

nonterminal Expr1_c with ast<Expr>, location, pp;
nonterminal Expr2_c with ast<Expr>, location, pp;
nonterminal Expr3_c with ast<Expr>, location, pp;
nonterminal Expr4_c with ast<Expr>, location, pp;

concrete production plus_c
top::Expr1_c ::= l::Expr1_c '+' r::Expr2_c
{
  top.ast = add(l.ast, r.ast, location=top.location);
  top.pp = cat(l.pp, cat(text(" + "), r.pp));
}

concrete production parens_c
top::Expr4_c ::= '(' e::Expr1_c ')'
{
  top.ast = e.ast;
  top.pp = parens(e.pp);
}

concrete production intTy_c
top::Expr4_c ::= e::'Int'
{
  top.ast = intTy(location=e.location);
  top.pp = text("Int");
}

concrete production typeKind_c
top::Expr4_c ::= e::'TYPE'
{
  top.ast = typeKind(location=e.location);
  top.pp = text("TYPE");
}

concrete production expr12_c
top::Expr1_c ::= e::Expr2_c {
  top.ast = e.ast;
  top.pp = e.pp;
}

concrete production expr23_c
top::Expr2_c ::= e::Expr3_c {
  top.ast = e.ast;
  top.pp = e.pp;
}

concrete production expr34_c
top::Expr3_c ::= e::Expr4_c {
  top.ast = e.ast;
  top.pp = e.pp;
}
