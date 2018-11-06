grammar edu:umn:cs:melt:minidep:driver;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit as implicit;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit only sigs;
import edu:umn:cs:melt:minidep:abstractsyntax:unification as unification;
import edu:umn:cs:melt:minidep:concretesyntax only Root_c, ast;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

function loadFile
IOMonad<Either<[Message] Decorated implicit:Decls>> ::= parse::(ParseResult<Root_c> ::= String String) path::String env::[Pair<String Maybe<Decorated implicit:Signature>>]
{
  return do (bindErrIO, returnErrIO) {
    cst :: Root_c <- mustParse(parse, path);
    liftIO(printM("cst pp:\n" ++ show(80, cst.pp)));
    throwIfAny(cst.errors);

    ast :: Decorated implicit:Decls = decorate cst.ast with {
      implicit:env = env;
    };
    liftIO(printM("\nast pp (pre-elaboration):\n" ++ show(80, ast.pp)));
    throwIfAny(ast.errors);

    return ast;
  };
}

function loadSigs
IOMonad<Either<[Message] [Pair<String Maybe<Decorated implicit:Signature>>]>> ::= parse::(ParseResult<Root_c> ::= String String) path::String env::[Pair<String Maybe<Decorated implicit:Signature>>]
{
  return do (bindErrIO, returnErrIO) {
    decls :: Decorated implicit:Decls <- loadFile(parse, path, env);
    return mapSndJust(decls.sigs);
  };
}

function loadAndElab
IOMonad<Either<[Message] Decorated unification:Decls>> ::= parse::(ParseResult<Root_c> ::= String String) path::String env::[Pair<String Maybe<Decorated implicit:Signature>>]
{
  return bindErrIO(loadFile(parse, path, env), elabAndSolve);
}
