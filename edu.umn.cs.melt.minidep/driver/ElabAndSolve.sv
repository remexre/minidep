grammar edu:umn:cs:melt:minidep:driver;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit as implicit;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit only elaboratedDecls, elaboratedExpr, env;
import edu:umn:cs:melt:minidep:abstractsyntax:unification as unification;
import edu:umn:cs:melt:minidep:abstractsyntax:unification only constraints, unificationVars, unified;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

function elabAndSolve
IOMonad<Either<[Message] Decorated unification:Decls>> ::= astPreElaboration::Decorated implicit:Decls
{
  return do (bindErrIO, returnErrIO) {
    unificationDefaultEnv :: [Pair<String Maybe<unification:Expr>>] = map(
      \p::Pair<String Maybe<Decorated implicit:Signature>> ->
        case p.snd of
        | just(s) -> pair(p.fst, just(s.elaboratedExpr))
        | nothing() -> pair(p.fst, nothing())
        end,
      astPreElaboration.env);

    astPreUnification :: Decorated unification:Decls =
      decorate astPreElaboration.elaboratedDecls with {
        unification:inhTyEnv = unificationDefaultEnv;
      };
    liftIO(printM("ast pp (pre-unification):\n" ++ show(80, astPreUnification.pp)));
    throwIfAny(astPreUnification.errors);

    liftIO(printM("ast constraints:\n" ++ show(80, ppConcat(map(
      \c::unification:Constraint -> cat(c.pp, line()),
      astPreUnification.constraints)))));
    astPostUnification :: Decorated unification:Decls =
      decorate astPreUnification.unified with {
        unification:inhTyEnv = unificationDefaultEnv;
      };
    liftIO(printM("\nast pp (post-unification):\n" ++ show(80, astPostUnification.pp)));
    throwIfAny(astPostUnification.errors);

    throwIfAny(map(
      \p::Pair<Integer Location> -> err(p.snd, "Couldn't solve for ?" ++ toString(p.fst) ++ "..."),
      astPostUnification.unificationVars));

    return astPostUnification;
  };
}
