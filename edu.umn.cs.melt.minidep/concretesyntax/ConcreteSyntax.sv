grammar edu:umn:cs:melt:minidep:concretesyntax;

import edu:umn:cs:melt:minidep:abstractsyntax:explicit;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

synthesized attribute ast<a> :: a;
synthesized attribute csts<a> :: [Decorated a];

-- The root nonterminal and associated productions.

nonterminal Root_c with ast<Decls>, errors, location, pp;

concrete production root_c
top::Root_c ::= decls::Decls_c
{
  top.ast = decls.ast;
  top.errors := decls.errors;
  top.pp = decls.pp;
}

-- The declaration, claim, and definition nonterminals and productions.

nonterminal Decls_c with ast<Decls>, env, errors, location, pp;

concrete production declsConsClaim_c
top::Decls_c ::= name::Name_t ':' ty::Expr1_c ';;' tl::Decls_c
{
  local tmpErr :: Pair<Decls [Message]> = pair(
    error("A claim must precede a definition of the same binding"),
    [err(top.location, "A claim must precede a definition of the same binding")]);
  local tmp :: Pair<Decls [Message]> = case tl of
  | declsConsDef_c(n, _, expr, _, tl2) ->
      if n.lexeme == name.lexeme
      then pair(
        declsCons(decl(name.lexeme, implicitsNil(location=top.location), ty.ast,
                       expr.ast, location=top.location),
                  tl2.ast),
        [])
      else tmpErr
  | _ -> tmpErr
  end;

  top.ast = tmp.fst;
  top.errors := tmp.snd;
  top.pp = ppConcat(
    [ text(name.lexeme)
    , text(" : ")
    , ty.pp
    , text(";;")
    , line()
    , tl.pp
    ]);
}

concrete production declsConsDef_c
top::Decls_c ::= name::Name_t '=' expr::Expr1_c ';;' tl::Decls_c
{
  top.ast = error("Definition without a corresponding claim?");
  top.errors := tl.errors;
  top.errors <- [err(top.location, "Definition without a corresponding claim?")];
  top.pp = ppConcat(
    [ text(name.lexeme)
    , text(" = ")
    , expr.pp
    , text(";;")
    , line()
    , tl.pp
    ]);
}

concrete production declsNil_c
top::Decls_c ::=
{
  top.ast = declsNil();
  top.errors := [];
  top.pp = notext();
}

-- The expression nonterminals.

nonterminal Expr1_c with ast<Expr>, env, location, pp;
nonterminal Expr2_c with ast<Expr>, env, location, pp;
nonterminal Expr3_c with ast<Expr>, env, location, pp;
nonterminal Expr4_c with ast<Expr>, env, location, pp;
nonterminal Expr5_c with ast<Expr>, env, location, pp;

concrete production lam_c
top::Expr1_c ::= '\' arg::Name_t '.' body::Expr1_c
{
  top.ast = lam(arg.lexeme, body.ast, location=top.location);
  top.pp = ppConcat(
    [ text("\\")
    , text(arg.lexeme)
    , text(". ")
    , body.pp
    ]);
}

concrete production arr_c
top::Expr1_c ::= l::Expr2_c '->' r::Expr1_c
{
  top.ast = pi(nothing(), l.ast, r.ast, location=top.location);
  top.pp = ppConcat([l.pp, text(" -> "), r.pp]);
}

concrete production pi_c
top::Expr1_c ::= 'Pi' arg::Name_t ':' ty::Expr2_c '.' body::Expr1_c
{
  top.ast = pi(just(arg.lexeme), ty.ast, body.ast, location=top.location);
  top.pp = ppConcat(
    [ text("Pi ")
    , text(arg.lexeme)
    , text(":")
    , ty.pp
    , text(". ")
    , body.pp
    ]);
}

{-
concrete production tyAnnot_c
top::Expr1_c ::= l::Expr2_c ':' r::Expr1_c
{
  -- top.ast = tyAnnot(l.ast, r.ast, location=top.location);
  top.pp = cat(l.pp, cat(text(" : "), r.pp));
}
-}

concrete production add_c
top::Expr2_c ::= l::Expr2_c '+' r::Expr3_c
{
  top.ast = app(app(var("(+)", location=top.location),
    l.ast, location=top.location),
    r.ast, location=top.location);
  top.pp = ppConcat([l.pp, text(" + "), r.pp]);
}

concrete production mul_c
top::Expr3_c ::= l::Expr3_c '*' r::Expr4_c
{
  top.ast = app(app(globalVar("(*)", location=top.location),
    l.ast, location=top.location),
    r.ast, location=top.location);
  top.pp = ppConcat([l.pp, text(" * "), r.pp]);
}

concrete production app_c
top::Expr4_c ::= l::Expr4_c r::Expr5_c
{
  top.ast = app(l.ast, r.ast, location=top.location);
  top.pp = ppConcat([l.pp, space(), r.pp]);
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
  local topCsts :: [Decorated Expr1_c] = cons(h, t.csts);
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
  top.ast = var(e.lexeme, implicitsNil(), location=top.location);
  top.pp = text(e.lexeme);
}

concrete production varImplicits_c
top::Expr5_c ::= e::Name_t '{' h::ImplicitVal_c t::ImplicitVals_c '}'
{
  top.ast = var(e.lexeme, implicitsCons(h.ast, t.ast), location=top.location);
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

nonterminal Expr1List_c with ast<[Expr]>, csts<Expr1_c>, env;

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

nonterminal ImplicitVal_c with ast<Pair<String Expr>>, env;

concrete production implicitVal_c
top::ImplicitVal_c ::= name::Name_t '=' expr::Expr1_c
{
  top.ast = error("TODO");
}

nonterminal ImplicitVals_c with ast<[Expr]>, csts<ImplicitVal_c>, env;

concrete production implicitsCons_c
top::ImplicitVals_c ::= ',' h::ImplicitVal_c t::ImplicitVals_c
{
  top.ast = cons(h.ast, t.ast);
  top.csts = cons(h, t.csts);
}

concrete production implicitsNil_c
top::ImplicitVals_c ::=
{
  top.ast = [];
  top.csts = [];
}
