grammar edu:umn:cs:melt:minidep:abstractsyntax:explicit;

import edu:umn:cs:melt:minidep:concretesyntax;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

aspect production declsCons
top::Decls ::= h::Decl t::Decls
{
  top.pp = cat(h.pp, t.pp);
}

aspect production declsNil
top::Decls ::=
{
  top.pp = notext();
}

aspect production decl
top::Decl ::= name::String implicits::Implicits ty::Expr body::Expr
{
  local implicitsPP :: Document = case implicits of
  | implicitsNil() -> notext()
  | _ -> ppImplode(text(", "),
      map(\p :: Pair<String Expr> -> ppConcat(
        [ text(p.fst)
        , text(" : ")
        , p.snd.expr1_c.pp
        ]), implicits.asList))
  end;
  top.pp = ppConcat(
    [ text(name)
    , implicitsPP
    , text(" : ")
    , ty.expr1_c.pp
    , text(";;")
    , line()
    , text(name)
    , text(" = ")
    , body.expr1_c.pp
    , text(";;")
    , line()
    , line()
    ]);
}

synthesized attribute expr1_c :: Expr1_c occurs on Expr;
synthesized attribute expr2_c :: Expr2_c occurs on Expr;
synthesized attribute expr3_c :: Expr3_c occurs on Expr;
synthesized attribute expr4_c :: Expr4_c occurs on Expr;
synthesized attribute expr5_c :: Expr5_c occurs on Expr;

aspect production app
top::Expr ::= l::Expr r::Expr
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = app_c(l.expr4_c, r.expr5_c, location=top.location);
  top.expr5_c = parens_c('(', top.expr1_c, ')', location=top.location);
}

aspect production globalVar
top::Expr ::= name::String implicits::Implicits
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = var_c(terminal(Name_t, name, top.location), location=top.location);
}

aspect production lam
top::Expr ::= name::String body::Expr
{
  top.expr1_c = lam_c('\', terminal(Name_t, name, top.location), '.',
    body.expr1_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = parens_c('(', top.expr1_c, ')', location=top.location);
}

aspect production localVar
top::Expr ::= name::String
{
  top.expr1_c = expr12_c(top.expr2_c, location=top.location);
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = var_c(terminal(Name_t, name, top.location), location=top.location);
}

aspect production pi
top::Expr ::= name::Maybe<String> l::Expr r::Expr
{
  top.expr1_c = case name of
  | just(name) -> pi_c('Pi', terminal(Name_t, name, top.location),
      ':', l.expr2_c, '.', r.expr1_c, location=top.location)
  | nothing() -> arr_c(l.expr2_c, '->', r.expr1_c, location=top.location)
  end;
  top.expr2_c = expr23_c(top.expr3_c, location=top.location);
  top.expr3_c = expr34_c(top.expr4_c, location=top.location);
  top.expr4_c = expr45_c(top.expr5_c, location=top.location);
  top.expr5_c = parens_c('(', top.expr1_c, ')', location=top.location);
}
