grammar edu:umn:cs:melt:minidep:driver;

import core:monad;

nonterminal Args;

abstract production argsCompileFile
top::Args ::= path::String
{}

function parseArgs
IOMonad<Args> ::= args::[String]
{
  -- TODO
  return exitM(1);
}
