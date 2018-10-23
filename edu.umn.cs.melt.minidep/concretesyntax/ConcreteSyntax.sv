grammar edu:umn:cs:melt:minidep:concretesyntax;

import edu:umn:cs:melt:minidep:abstractsyntax:implicit;
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
nonterminal Expr5_c with ast<Expr>, location, pp;

concrete production lam_c
top::Expr1_c ::= '\' arg::Name_t '.' body::Expr1_c
{
  top.ast = lam(arg.lexeme, body.ast, location=top.location);
  top.pp = cat(cat(text("\\"), text(arg.lexeme)), cat(text(". "), body.pp));
}

concrete production lamI_c
top::Expr1_c ::= '\' '{' arg::Name_t '}' '.' body::Expr1_c
{
  top.ast = lamI(arg.lexeme, body.ast, location=top.location);
  top.pp = cat(cat(text("\\"), text(arg.lexeme)), cat(text(". "), body.pp));
}

concrete production arr_c
top::Expr1_c ::= l::Expr2_c '->' r::Expr1_c
{
  top.ast = pi(nothing(), l.ast, r.ast, location=top.location);
  top.pp = cat(l.pp, cat(text(" -> "), r.pp));
}

concrete production pi_c
top::Expr1_c ::= 'Pi' arg::Name_t ':' ty::Expr2_c '.' body::Expr1_c
{
  top.ast = pi(just(arg.lexeme), ty.ast, body.ast, location=top.location);
  top.pp = cat(cat(text("Pi "), text(arg.lexeme)),
    cat(cat(text(":"), ty.pp),
        cat(text(". "), body.pp)));
}

concrete production piI_c
top::Expr1_c ::= 'Pi' '{' arg::Name_t ':' ty::Expr2_c '}' '.' body::Expr1_c
{
  top.ast = piI(arg.lexeme, ty.ast, body.ast, location=top.location);
  top.pp = cat(cat(text("Pi {"), text(arg.lexeme)),
    cat(cat(text(":"), ty.pp),
        cat(text("}. "), body.pp)));
}

concrete production tyAnnot_c
top::Expr1_c ::= l::Expr2_c ':' r::Expr1_c
{
  top.ast = tyAnnot(l.ast, r.ast, location=top.location);
  top.pp = cat(l.pp, cat(text(" : "), r.pp));
}

concrete production add_c
top::Expr2_c ::= l::Expr2_c '+' r::Expr3_c
{
  top.ast = add(l.ast, r.ast, location=top.location);
  top.pp = cat(l.pp, cat(text(" + "), r.pp));
}

concrete production mul_c
top::Expr3_c ::= l::Expr3_c '*' r::Expr4_c
{
  top.ast = mul(l.ast, r.ast, location=top.location);
  top.pp = cat(l.pp, cat(text(" * "), r.pp));
}

concrete production app_c
top::Expr4_c ::= l::Expr4_c r::Expr5_c
{
  top.ast = app(l.ast, r.ast, location=top.location);
  top.pp = cat(l.pp, cat(space(), r.pp));
}

concrete production appI_c
top::Expr4_c ::= l::Expr4_c '{' name::Name_t '=' r::Expr5_c '}'
{
  top.ast = appI(l.ast, name.lexeme, r.ast, location=top.location);
  top.pp = cat(l.pp, cat(space(), braces(cat(cat(text(name.lexeme), text("=")), r.pp))));
}

function expandList
Expr ::= es::[Expr] loc::Location
{
  local nilE :: Expr = var("nil", location=loc);
  local consE :: (Expr ::= Expr Expr) =
    \h::Expr t::Expr -> app(app(var("cons", location=loc), h, location=loc), t, location=loc);
  return foldr(consE, nilE, es);
}

concrete production nilList_c
top::Expr5_c ::= '[' ']'
{
  top.ast = var("nil", location=top.location);
  top.pp = text("[]");
}

concrete production nonNilList_c
top::Expr5_c ::= '[' h::Expr1_c t::Expr1List_c ']'
{
  local topAsts :: [Expr] = cons(h.ast, t.ast);
  local topCsts :: [Expr1_c] = cons(h, t.csts);
  top.ast = expandList(topAsts, top.location);
  top.pp = brackets(ppImplode(text(", "), map((.pp), topCsts)));
}

concrete production parens_c
top::Expr5_c ::= '(' e::Expr1_c ')'
{
  top.ast = e.ast;
  top.pp = parens(e.pp);
}

concrete production var_c
top::Expr5_c ::= e::Name_t
{
  top.ast = var(e.lexeme, location=top.location);
  top.pp = text(e.lexeme);
}

function expandNat
Expr ::= n::Integer loc::Location
{
  return if n == 0
         then var("zero", location=loc)
         else app(var("succ", location=loc), expandNat(n-1, loc), location=loc);
}

concrete production nat_c
top::Expr5_c ::= e::Nat_t
{
  top.ast = expandNat(toInt(e.lexeme), top.location);
  top.pp = text(e.lexeme);
}

concrete production typeKind_c
top::Expr5_c ::= 'TYPE'
{
  top.ast = typeKind(location=top.location);
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

concrete production expr45_c
top::Expr4_c ::= e::Expr5_c {
  top.ast = e.ast;
  top.pp = e.pp;
}

nonterminal Expr1List_c with ast<[Expr]>, csts;
synthesized attribute csts :: [Expr1_c];

concrete production expr1ListCons_c
top::Expr1List_c ::= ',' h::Expr1_c t::Expr1List_c
{
  top.ast = cons(h.ast, t.ast);
  top.csts = cons(h, t.csts);
}

concrete production expr1ListNil_c
top::Expr1List_c ::=
{
  top.ast = [];
  top.csts = [];
}
