grammar edu:umn:cs:melt:minidep:driver;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit as implicit;
import edu:umn:cs:melt:minidep:abstractsyntax:explicit as explicit;
import edu:umn:cs:melt:minidep:concretesyntax only Root_c, imps, root;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

function loadFile
IOMonad<Either<[Message] implicit:Root>> ::= parse::(ParseResult<Root_c> ::= String String) path::String alreadyLoading::[String]
{
  return if containsBy(stringEq, path, alreadyLoading) then
    throwOne(err(builtin(), "Dependency loop involving " ++ path))
  else do (bindErrIO, returnErrIO) {
    cst :: Root_c <- mustParse(parse, path);
    liftIO(printM("cst pp:\n" ++ show(80, cst.pp)));
    throwIfAny(cst.errors);

    deps :: [Pair<String implicit:Root>] <- loadFiles(parse, cst.imps, path::alreadyLoading);
    ast :: implicit:Root = cst.root(deps);
    liftIO(printM("\nast pp (pre-elaboration):\n" ++ show(80, ast.pp)));
    throwIfAny(ast.errors);

    return ast;
  };
}

function loadFiles
IOMonad<Either<[Message] [Pair<String implicit:Root>]>> ::= parse::(ParseResult<Root_c> ::= String String) paths::[String] alreadyLoading::[String]
{
  return case paths of
  | [] -> returnErrIO([])
  | h::t -> do (bindErrIO, returnErrIO) {
      h2 :: implicit:Root <- loadFile(parse, h, alreadyLoading);
      t2 :: [Pair<String implicit:Root>] <- loadFiles(parse, t, alreadyLoading);
      return (pair(h, h2) :: t2);
    }
  end;
}

function loadAndElab
IOMonad<Either<[Message] explicit:Root>> ::= parse::(ParseResult<Root_c> ::= String String) path::String
{
  return bindErrIO(loadFile(parse, path, []), elabAndSolve);
}
