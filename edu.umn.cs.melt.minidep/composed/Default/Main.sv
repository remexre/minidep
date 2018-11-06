grammar edu:umn:cs:melt:minidep:composed:Default;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit as implicit;
import edu:umn:cs:melt:minidep:abstractsyntax:unification as unification;
import edu:umn:cs:melt:minidep:abstractsyntax:unification only hasVars;
import edu:umn:cs:melt:minidep:concretesyntax only Root_c;
import edu:umn:cs:melt:minidep:driver;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp only show;

parser parse::Root_c
{
  edu:umn:cs:melt:minidep:concretesyntax;
}

function main
IOVal<Integer> ::= args::[String] ioIn::IO
{
  return evalErrIO(do (bindErrIO, returnErrIO) {
    if null(args) then {
      liftIO(printM("Usage: [minidep invocation] [filename]\n"));
      return 5;
    } else {
      stdlib :: [Pair<String Maybe<Decorated implicit:Signature>>] <-
        loadSigs(parse, "examples/stdlib.mdp", []);
      ast :: Decorated unification:Decls <- loadAndElab(parse, head(args), stdlib);

      -- TODO
      liftIO(printM("TODO: FINISH COMPILING!\n"));
      return 0;
    }
  }, ioIn);
}
