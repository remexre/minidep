grammar edu:umn:cs:melt:minidep:abstractsyntax:explicit;

function exprEq
Boolean ::= l::Expr r::Expr
{
  return case l, r of
  | lam(a, b), lam(c, d) -> a == c && exprEq(b, d)
  | pi(just(a), b, c), pi(just(d), e, f) -> a == d && exprEq(b, e) && exprEq(c, f)
  | pi(nothing(), a, b), pi(nothing(), c, d) -> exprEq(a, b) && exprEq(c, d)
  | tyAnnot(a, b), tyAnnot(c, d) -> exprEq(a, c) && exprEq(b, d)
  | add(a, b), add(c, d) -> exprEq(a, c) && exprEq(b, d)
  | mul(a, b), mul(c, d) -> exprEq(a, c) && exprEq(b, d)
  | app(a, b), app(c, d) -> exprEq(a, c) && exprEq(b, d)
  | var(just(a)), var(just(b)) -> a == b
  | var(nothing()), var(nothing()) -> true
  | nat(a), nat(b) -> a == b
  | natTy(), natTy() -> true
  | typeKind(), typeKind() -> true
  | _, _ -> false
  end;
}
