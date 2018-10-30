grammar edu:umn:cs:melt:minidep:concretesyntax;

import edu:umn:cs:melt:minidep:abstractsyntax:implicit;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

synthesized attribute ast<a> :: a;

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

nonterminal Decls_c with ast<Decls>, errors, location, pp;

concrete production declsConsClaim_c
top::Decls_c ::= name::Name_t ':' imps::ImplicitTys_c ty::Expr1_c ';' tl::Decls_c
{
  local tmpErr :: Pair<Decls [Message]> = pair(
    error("A claim must precede a definition of the same binding"),
    [err(top.location, "A claim must precede a definition of the same binding")]);
  local tmp :: Pair<Decls [Message]> = case tl of
  | declsConsDef_c(n, _, expr, _, tl2) ->
      if n.lexeme == name.lexeme
      then pair(
        declsCons(decl(name.lexeme, imps.ast, ty.ast, expr.ast, location=top.location),
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
    , imps.pp
    , ty.pp
    , text(";")
    , line()
    , tl.pp
    ]);
}

concrete production declsConsDef_c
top::Decls_c ::= name::Name_t '=' expr::Expr1_c ';' tl::Decls_c
{
  top.ast = error("Definition without a corresponding claim?");
  top.errors := tl.errors;
  top.errors <- [err(top.location, "Definition without a corresponding claim?")];
  top.pp = ppConcat(
    [ text(name.lexeme)
    , text(" = ")
    , expr.pp
    , text(";")
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

nonterminal Expr1_c with ast<Expr>, location, pp;
nonterminal Expr2_c with ast<Expr>, location, pp;
nonterminal Expr3_c with ast<Expr>, location, pp;
nonterminal Expr4_c with ast<Expr>, location, pp;
nonterminal Expr5_c with ast<Expr>, location, pp;

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
    , text(": ")
    , ty.pp
    , text(". ")
    , body.pp
    ]);
}

concrete production tyAnnot_c
top::Expr1_c ::= l::Expr2_c ':' r::Expr1_c
{
  top.ast = app(app(var("(:)", implicitsNil(location=top.location),
                        location=top.location),
    l.ast, location=top.location),
    r.ast, location=top.location);
  top.pp = cat(l.pp, cat(text(" : "), r.pp));
}

concrete production add_c
top::Expr2_c ::= l::Expr2_c '+' r::Expr3_c
{
  top.ast = app(app(var("(+)", implicitsNil(location=top.location),
                        location=top.location),
    l.ast, location=top.location),
    r.ast, location=top.location);
  top.pp = ppConcat([l.pp, text(" + "), r.pp]);
}

concrete production mul_c
top::Expr3_c ::= l::Expr3_c '*' r::Expr4_c
{
  top.ast = app(app(var("(*)", implicitsNil(location=top.location),
                        location=top.location),
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
  local nilE :: Expr = var("nil", implicitsNil(location=builtin()), location=loc);
  local consE :: (Expr ::= Expr Expr) =
    \h::Expr t::Expr -> app(app(var("cons", implicitsNil(location=builtin()),
                                    location=loc),
                                h, location=loc),
                            t, location=loc);
  return foldr(consE, nilE, es);
}

concrete production nilList_c
top::Expr5_c ::= '[' ']'
{
  top.ast = var("nil", implicitsNil(location=builtin()), location=top.location);
  top.pp = text("[]");
}

concrete production nonNilList_c
top::Expr5_c ::= '[' h::Expr1_c t::Expr1List_c ']'
{
  top.ast = expandList(cons(h.ast, t.ast), top.location);
  top.pp = brackets(ppImplode(text(", "), map((.pp), cons(h, t.exprCsts))));
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
  top.ast = var(e.lexeme, implicitsNil(location=top.location), location=top.location);
  top.pp = text(e.lexeme);
}

concrete production varImplicits_c
top::Expr5_c ::= e::Name_t '{' h::ImplicitVal_c t::ImplicitVals_c '}'
{
  top.ast = var(e.lexeme,
                implicitsCons(h.ast.fst, h.ast.snd, t.ast,
                              location=top.location),
                location=top.location);
  top.pp = ppConcat(
    [ text(e.lexeme)
    , space()
    , braces(ppImplode(text(", "), map((.pp), cons(h, t.implicitValCsts))))
    ]);
}

function expandNat
Expr ::= n::Integer loc::Location
{
  return if n == 0
         then var("zero", implicitsNil(location=loc), location=loc)
         else app(var("succ", implicitsNil(location=loc), location=loc),
                  expandNat(n-1, loc),
                  location=loc);
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

nonterminal Expr1List_c with ast<[Expr]>, exprCsts;
synthesized attribute exprCsts :: [Decorated Expr1_c];

concrete production expr1ListCons_c
top::Expr1List_c ::= ',' h::Expr1_c t::Expr1List_c
{
  top.ast = cons(h.ast, t.ast);
  top.exprCsts = cons(h, t.exprCsts);
}

concrete production expr1ListNil_c
top::Expr1List_c ::=
{
  top.ast = [];
  top.exprCsts = [];
}

nonterminal ImplicitTy_c with ast<Pair<String Expr>>, pp;

concrete production implicitTy_c
top::ImplicitTy_c ::= name::Name_t ':' expr::Expr1_c
{
  top.ast = pair(name.lexeme, expr.ast);
  top.pp = ppConcat(
    [ text(name.lexeme)
    , text(": ")
    , expr.pp
    ]);
}

nonterminal ImplicitTys_c with ast<Implicits>, implicitTyCsts, location, pp;
synthesized attribute implicitTyCsts :: [Decorated ImplicitTy_c];

concrete production implicitTysNone_c
top::ImplicitTys_c ::=
{
  top.ast = implicitsNil(location=top.location);
  top.implicitTyCsts = nil();
  top.pp = notext();
}

concrete production implicitTysSome_c
top::ImplicitTys_c ::= '{' h::ImplicitTy_c t::ImplicitTyList_c '}'
{
  top.ast = implicitsCons(h.ast.fst, h.ast.snd, t.ast, location=top.location);
  top.implicitTyCsts = cons(h, t.implicitTyCsts);
  top.pp = cat(braces(cat(h.pp, t.pp)), space());
}

nonterminal ImplicitTyList_c with ast<Implicits>, implicitTyCsts, location, pp;

concrete production implicitTysCons_c
top::ImplicitTyList_c ::= ',' h::ImplicitTy_c t::ImplicitTyList_c
{
  top.ast = implicitsCons(h.ast.fst, h.ast.snd, t.ast, location=top.location);
  top.implicitTyCsts = cons(h, t.implicitTyCsts);
  top.pp = ppConcat(
    [ text(", ")
    , h.pp
    , t.pp
    ]);
}

concrete production implicitTysNil_c
top::ImplicitTyList_c ::=
{
  top.ast = implicitsNil(location=top.location);
  top.implicitTyCsts = [];
  top.pp = notext();
}

nonterminal ImplicitVal_c with ast<Pair<String Expr>>, pp;

concrete production implicitVal_c
top::ImplicitVal_c ::= name::Name_t '=' expr::Expr1_c
{
  top.ast = pair(name.lexeme, expr.ast);
  top.pp = ppConcat(
    [ text(name.lexeme)
    , text(" = ")
    , expr.pp
    ]);
}

nonterminal ImplicitVals_c with ast<Implicits>, implicitValCsts, location;
synthesized attribute implicitValCsts :: [Decorated ImplicitVal_c];

concrete production implicitValsCons_c
top::ImplicitVals_c ::= ',' h::ImplicitVal_c t::ImplicitVals_c
{
  top.ast = implicitsCons(h.ast.fst, h.ast.snd, t.ast, location=top.location);
  top.implicitValCsts = cons(h, t.implicitValCsts);
}

concrete production implicitValsNil_c
top::ImplicitVals_c ::=
{
  top.ast = implicitsNil(location=top.location);
  top.implicitValCsts = [];
}
