grammar edu:umn:cs:melt:minidep:concretesyntax;

import edu:umn:cs:melt:minidep:abstractsyntax:implicit;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

-- The root nonterminal and associated productions.

nonterminal Root_c with errors, imps, location, pp, root;
synthesized attribute imps :: [String];
synthesized attribute root :: (Root ::= [Pair<String Root>]);

concrete production root_c
top::Root_c ::= imps::Imports_c decls::Decls_c
{
  top.errors := decls.errors;
  top.imps = imps.imps;
  top.pp = cat(imps.pp, decls.pp);
  top.root = root(_, decls.ast);
}

-- The import nonterminal.

nonterminal Import_c with location, name, pp;
synthesized attribute name :: String;

concrete production import_c
top::Import_c ::= 'import' name::String_c ';'
{
  top.name = name.string;
  top.pp = ppConcat(
    [ text("import \"")
    , text(name.string) -- TODO: Escapes aren't handled, but _hopefully_
                        -- there're no escapes in your filenames, either.
    , text("\";")
    ]);
}

nonterminal Imports_c with errors, imps, location, pp;

concrete production importsCons_c
top::Imports_c ::= hd::Import_c tl::Imports_c
{
  top.errors := tl.errors;
  top.errors <- if containsBy(stringEq, hd.name, tl.imps)
                then [err(hd.location, "Duplicate import")]
                else [];
  top.imps = hd.name :: tl.imps;
  top.pp = ppConcat(
    [ hd.pp
    , line()
    , tl.pp
    ]);
}

concrete production importsNil_c
top::Imports_c ::=
{
  top.errors := [];
  top.imps = [];
  top.pp = line();
}

-- The declaration, claim, and definition nonterminals and productions.

nonterminal Decls_c with ast<Decls>, errors, location, pp;
synthesized attribute ast<a> :: a;

-- TODO: Use `layout` to not need semicolons
-- http://melt.cs.umn.edu/silver/ref/decl/productions/concrete/#layout
concrete production declsConsClaim_c
top::Decls_c ::= name::Name_t ':' imps::ImplicitTys_c ty::Expr1_c ';' tl::Decls_c
{
  top.ast = case tl of
  | declsConsDef_c(n, _, expr, _, tl2) ->
      if n.lexeme == name.lexeme then
        declsCons(declDef(name.lexeme, imps.ast, ty.ast, expr.ast, location=top.location),
                  tl2.ast)
      else
        declsCons(declDecl(name.lexeme, imps.ast, ty.ast, location=top.location), tl.ast)
  | _ -> declsCons(declDecl(name.lexeme, imps.ast, ty.ast, location=top.location), tl.ast)
  end;
  top.errors := case tl of
  | declsConsDef_c(n, _, expr, _, tl2) -> if n.lexeme == name.lexeme then tl2.errors else tl.errors
  | _ -> tl.errors
  end;

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
  local errMsg :: String = "Definition of " ++ name.lexeme ++ " without a corresponding claim?";
  top.ast = error(errMsg);
  top.errors := tl.errors;
  top.errors <- [err(top.location, errMsg)];
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
top::Expr1_c ::= '\' args::Args_c '->' body::Expr1_c
{
  top.ast = args.expand(body.ast, top.location);
  top.pp = ppConcat(
    [ text("\\")
    , args.pp
    , text(" -> ")
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
top::Expr1_c ::= '(' arg::Name_t ':' ty::Expr1_c ')' '->' body::Expr1_c
{
  top.ast = pi(just(arg.lexeme), ty.ast, body.ast, location=top.location);
  top.pp = ppConcat(
    [ text("(")
    , text(arg.lexeme)
    , text(": ")
    , ty.pp
    , text(") -> ")
    , body.pp
    ]);
}

concrete production eq_c
top::Expr1_c ::= l::Expr2_c '=' r::Expr1_c
{
  top.ast = app(app(var("(=)", implicitsNil(location=top.location),
                        location=top.location),
    l.ast, location=top.location),
    r.ast, location=top.location);
  top.pp = ppConcat([l.pp, text(" = "), r.pp]);
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

concrete production anon_c
top::Expr5_c ::= '_'
{
  top.ast = anon(location=top.location);
  top.pp = text("_");
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
top::Expr2_c ::= e::Name_t '{' h::ImplicitVal_c t::ImplicitVals_c '}'
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

concrete production type_c
top::Expr5_c ::= 'TYPE'
{
  top.ast = universe(location=top.location);
  top.pp = text("TYPE");
}

concrete production expr12_c
top::Expr1_c ::= e::Expr2_c
{
  top.ast = e.ast;
  top.pp = e.pp;
}

concrete production expr23_c
top::Expr2_c ::= e::Expr3_c
{
  top.ast = e.ast;
  top.pp = e.pp;
}

concrete production expr34_c
top::Expr3_c ::= e::Expr4_c
{
  top.ast = e.ast;
  top.pp = e.pp;
}

concrete production expr45_c
top::Expr4_c ::= e::Expr5_c
{
  top.ast = e.ast;
  top.pp = e.pp;
}

nonterminal String_c with location, string;
synthesized attribute string :: String;

concrete production string_c
top::String_c ::= '"' chars::StringChars_c '"'
layout {}
{
  top.string = chars.string;
}

nonterminal StringChars_c with location, string;

concrete production stringCharsConsChr_c
top::StringChars_c ::= hd::StrChr_t tl::StringChars_c
layout {}
{
  top.string = hd.lexeme ++ tl.string;
}

concrete production stringCharsConsEsc_c
top::StringChars_c ::= hd::StrEsc_t tl::StringChars_c
layout {}
{
  top.string = case hd.lexeme of
  | "\\\\" -> "\\"
  | "\\n"  -> "\n"
  | "\\r"  -> "\r"
  | "\\t"  -> "\t"
  | esc -> error("Unknown string escape: " ++ esc)
  end ++ tl.string;
}

concrete production stringCharsNil_c
top::StringChars_c ::=
{
  top.string = "";
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
top::ImplicitTys_c ::= '{' h::ImplicitTy_c t::ImplicitTyList_c '}' '->'
{
  top.ast = implicitsCons(h.ast.fst, h.ast.snd, t.ast, location=top.location);
  top.implicitTyCsts = cons(h, t.implicitTyCsts);
  top.pp = cat(braces(cat(h.pp, t.pp)), text(" -> "));
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

nonterminal Args_c with expand, pp;
synthesized attribute expand :: (Expr ::= Expr Location);

concrete production argsConsAnon_c
top::Args_c ::= '_' t::Args_c
{
  top.expand = \e::Expr l::Location -> lam(genSym(), t.expand(e, l), location=l);
  top.pp = cat(text("_"), cat(space(), t.pp));
}

concrete production argsConsArg_c
top::Args_c ::= h::Name_t t::Args_c
{
  top.expand = \e::Expr l::Location -> lam(h.lexeme, t.expand(e, l), location=l);
  top.pp = cat(text(h.lexeme), cat(space(), t.pp));
}

concrete production argsNilAnon_c
top::Args_c ::= '_'
{
  top.expand = \e::Expr l::Location -> lam(genSym(), e, location=l);
  top.pp = text("_");
}

concrete production argsNilArg_c
top::Args_c ::= a::Name_t
{
  top.expand = \e::Expr l::Location -> lam(a.lexeme, e, location=l);
  top.pp = text(a.lexeme);
}
