grammar edu:umn:cs:melt:minidep:util;

import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treeset as set;

synthesized attribute asList<a> :: [a];

function lookupTyEnv
Maybe<a> ::= name::String env::[Pair<String Maybe<a>>]
{
  return case lookupBy(stringEq, name, env) of
  | just(just(x)) -> just(x)
  | _ -> nothing()
  end;
}

function mapSndJust
[Pair<a Maybe<b>>] ::= l::[Pair<a b>]
{
  return map(\p::Pair<a b> -> pair(p.fst, just(p.snd)), l);
}

function nestErrors
[Message] ::= loc::Location doc::Document msgs::[Message]
{
  return if null(msgs) then
    []
  else
    [nested(loc, show(80, cat(text("In expression "), doc)), msgs)];
}

function set1
set:Set<String> ::= a::String
{
  return set:add([a], set:empty(compareString));
}
