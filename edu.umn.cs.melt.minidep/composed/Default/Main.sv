grammar edu:umn:cs:melt:minidep:composed:Default;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax:explicit as explicit;
import edu:umn:cs:melt:minidep:concretesyntax only Root_c;
import edu:umn:cs:melt:minidep:translation:utlc as utlc;
import edu:umn:cs:melt:minidep:translation:utlc only translateUTLC;
import edu:umn:cs:melt:minidep:driver;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp only show;

parser parse::Root_c
{
  edu:umn:cs:melt:minidep:concretesyntax;
  -- edu:umn:cs:melt:minidep:exts:types;
}

function main
IOVal<Integer> ::= args::[String] ioIn::IO
{
  return evalErrIO(do (bindErrIO, returnErrIO) {
    if null(args) then {
      liftIO(printM("Usage: [minidep invocation] [filename]\n"));
      return 5;
    } else {
      ast :: explicit:Root <- loadAndElab(parse, head(args));

      astUTLC :: utlc:Root = ast.translateUTLC;
      liftIO(printM(show(80, astUTLC.pp)));

      -- TODO
      liftIO(printM("TODO: FINISH COMPILING!\n"));
      return 0;
    }
  }, ioIn);
}
