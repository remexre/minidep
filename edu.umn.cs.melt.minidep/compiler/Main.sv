grammar edu:umn:cs:melt:minidep:compiler;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax;
import edu:umn:cs:melt:minidep:concretesyntax;
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
      fileName::String = head(args);
      src::String <- readFileM(fileName);
      result::ParseResult<Root_c> = parse(src, fileName);
      if !result.parseSuccess then {
        printM(result.parseErrors ++ "\n");
        return 2;
      } else {
        ast::Root = result.parseTree.ast;
        printM("pp: " ++ ast.pp.result ++ "\n");
        if null(ast.errors) then {
          -- printM("value: " ++ ast.value.pp.result ++ "\n");
          printM("ok\n");
          return 0;
        } else {
          printM(messagesToString(ast.errors) ++ "\n");
          return 1;
        }
      }
    }
  }, ioIn);
}
