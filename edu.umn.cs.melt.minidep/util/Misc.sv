grammar edu:umn:cs:melt:minidep:util;

function isJust
Boolean ::= a::Maybe<a>
{
  return case a of
  | just(_) -> true
  | nothing() -> false
  end;
}

function isNothing
Boolean ::= a::Maybe<a>
{
  return case a of
  | just(_) -> false
  | nothing() -> true
  end;
}
