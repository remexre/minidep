grammar edu:umn:cs:melt:minidep:abstractsyntax;

import silver:langutil;

function checkType
[Message] ::= ex::Expr ty::Expr
{
  return if exprEq(ex.synTy, ty) then
    []
  else
    [error("TODO Type Error")];
}
