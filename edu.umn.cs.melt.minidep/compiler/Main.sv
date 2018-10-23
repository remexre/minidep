grammar edu:umn:cs:melt:minidep:compiler;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit;
import edu:umn:cs:melt:minidep:concretesyntax;
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
        printM("cst pp: " ++ show(80, result.parseTree.pp) ++ "\n");
        ast :: Decorated Root = decorate result.parseTree.ast with {
          atoms = [
            pair("List", pi(nothing(), typeKind(location=builtin()),
                                       typeKind(location=builtin()),
                                       location=builtin())),
            pair("Nat", typeKind(location=builtin())),
            pair("nil", pi(nothing(), typeKind(location=builtin()),
                                       typeKind(location=builtin()),
                                       location=builtin()))
          ];
          env = [];
        };
        printM("ast pp: " ++ show(80, ast.pp) ++ "\n");
        if null(ast.errors) then {
          -- printM("value: " ++ ppsExpr(ast.normalized) ++ "\n");
          return 0;
        } else {
          printM(messagesToString(ast.errors) ++ "\n");
          return 1;
        }
      }
    }
  }, ioIn);
}
