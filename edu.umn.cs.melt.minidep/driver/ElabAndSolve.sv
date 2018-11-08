grammar edu:umn:cs:melt:minidep:driver;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit as implicit;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit only elaborated;
import edu:umn:cs:melt:minidep:abstractsyntax:unification as unification;
import edu:umn:cs:melt:minidep:abstractsyntax:unification only unificationVars, unified;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;
import silver:util:raw:treeset as set;

function elabAndSolve
IOMonad<Either<[Message] unification:Root>> ::= astPreElaboration::implicit:Root
{
  return do (bindErrIO, returnErrIO) {
    astPreUnification :: unification:Root = astPreElaboration.elaborated;
    liftIO(printM("ast pp (pre-unification):\n" ++ show(80, astPreUnification.pp)));
    throwIfAny(astPreUnification.errors);

    astPostUnification :: unification:Root = astPreUnification.unified;
    liftIO(printM("\nast pp (post-unification):\n" ++ show(80, astPostUnification.pp)));
    throwIfAny(astPostUnification.errors);

    throwIfAny(map(
      \p::Pair<Integer Location> -> err(p.snd, "Couldn't solve for ?" ++ toString(p.fst) ++ "..."),
      set:toList(astPostUnification.unificationVars)));

    return astPostUnification;
  };
}
