grammar edu:umn:cs:melt:minidep:util;

import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treeset as set;

synthesized attribute asList<a> :: [a];

function compareFstInteger
Integer ::= l::Pair<Integer a> r::Pair<Integer b>
{
  return l.fst - r.fst;
}

function debug
a ::= x::a pp::(Document ::= a)
{
  return unsafeTrace(x, print(show(80, cat(pp(x), line())), unsafeIO()));
}

function genSym
String ::=
{
  return "gensym#" ++ toString(genInt());
}

function lookupEnv
Maybe<a> ::= name::String env::[Pair<String Maybe<a>>]
{
  return case lookupBy(stringEq, name, env) of
  | just(just(x)) -> just(x)
  | _ -> nothing()
  end;
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
