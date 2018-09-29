grammar edu:umn:cs:melt:minidep:abstractsyntax;

function exprEq
Boolean ::= l::Expr r::Expr
{
  return case l, r of
  | add(a, b), add(c, d) -> exprEq(a, c) || exprEq(b, d)
  | natTy(), natTy() -> true
  | typeKind(), typeKind() -> true
  | _, _ -> false
  end;
}
