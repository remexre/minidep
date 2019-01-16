grammar edu:umn:cs:melt:minidep:exts:types;

import edu:umn:cs:melt:minidep:abstractsyntax:implicit as implicit;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit only Decls, declDecl, declsCons;
import edu:umn:cs:melt:minidep:concretesyntax;
import silver:langutil;
import silver:langutil:pp;

terminal Type_t  'type'  lexer classes {KEYWORD};
terminal Where_t 'where' lexer classes {KEYWORD};
terminal End_t   'end'   lexer classes {KEYWORD};

concrete production declsConsType_c
top::Decls_c ::= 'type' tyName::Name_t ':' tyTy::ImplicitTys_c 'where' ctors::Claims_c 'end' ';' tl::Decls_c
{
  top.ast = declsCons(declDecl(name.lexeme, imps.ast, ty.ast, location=top.location),
    ctors.ast(declDecl("elim" ++ name.lexeme, elimImps, elimTy, location=top.location), tl.ast));
  top.errors := ctors.errors ++ tl.errors;
  top.pp = ppConcat(
    [ text("type ")
    , text(tyName.lexeme)
    , text(" : ")
    , tyTy.pp
    , text(" where")
    , line()
    , ctors.pp
    , text("end;")
    , line()
    , tl.pp
    ]);
}

nonterminal Claims_c with ast<(Decls ::= Decls)>, errors, location, names, pp;
synthesized attribute names :: [String];

concrete production claimsCons_c
top::Claims_c ::= name::Name_t ':' imps::ImplicitTys_c ty::Expr1_c ';' tl::Claims_c
{
  top.ast = \lst::Decls -> declsCons(
    declDecl(name.lexeme, imps.ast, ty.ast, location=top.location), tl.ast(lst));
  top.errors := tl.errors;
  top.errors <- if containsBy(stringEq, name.lexeme, tl.names)
                then [err(top.location, "Duplicate constructor " ++ name.lexeme)]
                else [];
  top.names = name.lexeme :: tl.names;
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

concrete production claimsNil_c
top::Claims_c ::=
{
  top.ast = \lst::Decls -> lst;
  top.errors := [];
  top.names = [];
  top.pp = notext();
}
