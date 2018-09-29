grammar edu:umn:cs:melt:minidep:abstractsyntax;

import edu:umn:cs:melt:minidep:concretesyntax;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;

nonterminal Root with errors, location, pp;
nonterminal Expr with errors, expr1, expr2, expr3, expr4, inhTy, location, normalized, synTy, tyEnv;

inherited attribute inhTy :: Maybe<Expr>;
inherited attribute tyEnv :: [Pair<String Expr>];
synthesized attribute errors :: [Message] with ++;
synthesized attribute normalized :: Expr;
synthesized attribute synTy :: Expr;

abstract production root
top::Root ::= e::Expr
{
  top.errors := e.errors;
  top.pp = e.expr1.pp;
}

-- Operator productions.

abstract production add
top::Expr ::= l::Expr r::Expr
{
  l.tyEnv = top.tyEnv;
  r.tyEnv = top.tyEnv;

  top.errors := l.errors ++ r.errors;
  top.errors <- checkType(l, natTy(location=builtin()));
  top.errors <- checkType(r, natTy(location=builtin()));
  top.synTy = natTy(location=builtin());

  top.normalized = case l.normalized, r.normalized of
  | nat(l), nat(r) -> nat(l + r, location=builtin())
  | _, _ -> error("type error")
  end;
}

abstract production mul
top::Expr ::= l::Expr r::Expr
{
  l.tyEnv = top.tyEnv;
  r.tyEnv = top.tyEnv;

  top.errors := l.errors ++ r.errors;
  top.errors <- checkType(l, natTy(location=builtin()));
  top.errors <- checkType(r, natTy(location=builtin()));
  top.synTy = natTy(location=builtin());

  top.normalized = case l.normalized, r.normalized of
  | nat(l), nat(r) -> nat(l * r, location=builtin())
  | _, _ -> error("type error")
  end;
}

abstract production tyAnnot
top::Expr ::= l::Expr r::Expr
{
  l.tyEnv = top.tyEnv;
  r.tyEnv = top.tyEnv;

  top.errors := l.errors ++ r.errors;
  top.errors <- checkType(l, r.normalized);
  top.synTy = r.normalized;

  top.normalized = l.normalized;
}

-- Literal and Identifier productions.

abstract production nat
top::Expr ::= n::Integer -- RIP no unsigned ints
{
  top.errors := if n < 0 then [error("Somehow a negative number got in here?")] else [];
  top.synTy = natTy(location=builtin());
  top.normalized = top;
}

abstract production natTy
top::Expr ::=
{
  top.errors := [];
  top.synTy = typeKind(location=builtin());
  top.normalized = top;
}

abstract production typeKind
top::Expr ::=
{
  top.errors := [];
  top.synTy = error("Universe Polymorphism is hard");
  top.normalized = top;
}
