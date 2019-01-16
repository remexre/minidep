grammar edu:umn:cs:melt:minidep:translation:utlc;

import edu:umn:cs:melt:minidep:abstractsyntax:explicit as explicit;
import silver:langutil;
import silver:langutil:pp;

synthesized attribute translateUTLC<a> :: a;

attribute translateUTLC<Root> occurs on explicit:Root;

aspect production explicit:root
top::explicit:Root ::= decls::explicit:Decls
{
  top.translateUTLC = root(decls.translateUTLC);
}

attribute translateUTLC<Decls> occurs on explicit:Decls;

aspect production explicit:declsCons
top::explicit:Decls ::= h::explicit:Decl t::explicit:Decls
{
  local body :: explicit:Expr = case h of
  | explicit:declDef(_, _, body) -> body
  | _ -> error("unreachable")
  end;
  body.locals = [];

  top.translateUTLC = case h of
  | explicit:declDecl(_, _) -> t.translateUTLC
  | explicit:declDef(name, _, _) -> declsCons(name, body.translateUTLC, t.translateUTLC)
  end;
}

aspect production explicit:declsNil
top::explicit:Decls ::=
{
  top.translateUTLC = declsNil();
}

attribute translateUTLC<Expr> occurs on explicit:Expr;
inherited attribute locals :: [String] occurs on explicit:Expr;

function getLocal
Expr ::= name::String env::[String] loc::Location
{
  return case env of
  | h::t -> if h == name
            then localVar(0, location=loc)
            else case getLocal(name, t, loc) of
            | localVar(n) -> localVar(n + 1, location=loc)
            | e -> e
            end
  | [] -> globalVar(name, location=loc)
  end;
}

aspect production explicit:app
top::explicit:Expr ::= f::explicit:Expr x::explicit:Expr
{
  f.locals = top.locals;
  x.locals = top.locals;
  top.translateUTLC = app(f.translateUTLC, x.translateUTLC, location=top.location);
}

aspect production explicit:lam
top::explicit:Expr ::= name::String argTy::explicit:Expr body::explicit:Expr
{
  body.locals = name :: top.locals;
  top.translateUTLC = lam(body.translateUTLC, location=top.location);
}

aspect production explicit:pi
top::explicit:Expr ::= name::Maybe<String> l::explicit:Expr r::explicit:Expr
{
  top.translateUTLC = error("Cannot translate " ++ show(80, top.pp) ++ " to UTLC");
}

aspect production explicit:universe
top::explicit:Expr ::=
{
  top.translateUTLC = error("Cannot translate " ++ show(80, top.pp) ++ " to UTLC");
}

aspect production explicit:var
top::explicit:Expr ::= s::String
{
  top.translateUTLC = getLocal(s, top.locals, top.location);
}
