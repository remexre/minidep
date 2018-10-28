grammar edu:umn:cs:melt:minidep:compiler;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax:explicit;
import edu:umn:cs:melt:minidep:concretesyntax as cst;
import edu:umn:cs:melt:minidep:concretesyntax only Root_c, ast;
import edu:umn:cs:melt:minidep:util;
import silver:langutil;
import silver:langutil:pp;

parser parse::Root_c
{
  edu:umn:cs:melt:minidep:concretesyntax;
}

function main
IOVal<Integer> ::= args::[String] ioIn::IO
{
  local defaultEnv :: [Signature] = [
    sig("Nat", implicitsNil(location=builtin()), var("TYPE", location=builtin())),
    sig("zero", implicitsNil(location=builtin()), var("Nat", location=builtin())),
    sig("succ", implicitsNil(location=builtin()),
      pi(nothing(), var("Nat", location=builtin()),
                    var("Nat", location=builtin()),
                    location=builtin())),
    sig("List", implicitsNil(location=builtin()),
      pi(nothing(), var("TYPE", location=builtin()),
                    var("TYPE", location=builtin()),
                    location=builtin())),
    sig("nil", implicitsCons("T", var("TYPE", location=builtin()), implicitsNil(location=builtin()), location=builtin()),
      app(var("List", location=builtin()),
          var("T", location=builtin()),
          location=builtin())),
    sig("cons", implicitsCons("T", var("TYPE", location=builtin()), implicitsNil(location=builtin()), location=builtin()),
      pi(nothing(), var("T", location=builtin()),
                    pi(nothing(), app(var("List", location=builtin()),
                                      var("T", location=builtin()),
                                      location=builtin()),
                                  app(var("List", location=builtin()),
                                      var("T", location=builtin()),
                                      location=builtin()),
                                  location=builtin()),
                    location=builtin()))
  ];
  return evalIO(do (bindIO, returnIO) {
    if null(args) then {
      printM("Usage: [minidep invocation] [filename]\n");
      return 5;
    } else {
      fileName :: String = head(args);
      src :: String <- readFileM(fileName);
      result :: ParseResult<Root_c> = parse(src, fileName);
      if !result.parseSuccess then {
        printM(result.parseErrors ++ "\n");
        return 2;
      } else {
        cst :: Decorated Root_c = decorate result.parseTree with {
          cst:env = map(\s :: Signature -> error("TODO"), defaultEnv);
        };
        if !null(cst.errors) then {
          printM(messagesToString(result.parseTree.errors) ++ "\n");
          return 1;
        } else {
          printM("cst pp:\n" ++ show(80, result.parseTree.pp));
          ast :: Decorated Decls = decorate result.parseTree.ast with {
          };
          printM("ast pp:\n" ++ show(80, ast.pp));
          if !null(ast.errors) then {
            printM(messagesToString(ast.errors) ++ "\n");
            return 1;
          } else {
            -- printM("value: " ++ ppsExpr(ast.normalized) ++ "\n");
            return 0;
          }
        }
      }
    }
  }, ioIn);
}
