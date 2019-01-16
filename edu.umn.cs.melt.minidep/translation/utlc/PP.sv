grammar edu:umn:cs:melt:minidep:translation:utlc;

import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

aspect production root
top::Root ::= decls::Decls
{
  top.pp = decls.pp;
}

aspect production declsCons
top::Decls ::= n::String e::Expr t::Decls
{
  top.pp = ppConcat(
    [ text(n)
    , text(" = ")
    , e.pp
    , line()
    , t.pp
    ]);
}

aspect production declsNil
top::Decls ::=
{
  top.pp = notext();
}

aspect production app
top::Expr ::= f::Expr x::Expr
{
  top.pp = parens(ppImplode(space(), [f.pp, x.pp]));
}

aspect production lam
top::Expr ::= body::Expr
{
  top.pp = cat(text("\\"), body.pp);
}

aspect production globalVar
top::Expr ::= s::String
{
  top.pp = text(s);
}

aspect production localVar
top::Expr ::= n::Integer
{
  top.pp = text(toString(n));
}
