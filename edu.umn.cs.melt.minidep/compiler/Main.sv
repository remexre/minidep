grammar edu:umn:cs:melt:minidep:compiler;

import core:monad;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit;
import edu:umn:cs:melt:minidep:abstractsyntax:implicit as implicit;
import edu:umn:cs:melt:minidep:abstractsyntax:unification as unification;
import edu:umn:cs:melt:minidep:abstractsyntax:unification only constraints, hasVars, unified;
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
  local defaultEnv :: [Pair<String implicit:Signature>] =
    [ pair("Nat", sig(implicitsNil(location=builtin()),
        var("TYPE", implicitsNil(location=builtin()), location=builtin())))
    , pair("zero", sig(implicitsNil(location=builtin()),
        var("Nat",  implicitsNil(location=builtin()),location=builtin())))
    , pair("succ", sig(implicitsNil(location=builtin()),
        pi(nothing(), var("Nat", implicitsNil(location=builtin()), location=builtin()),
                      var("Nat", implicitsNil(location=builtin()), location=builtin()),
                      location=builtin())))
    , pair("List", sig(implicitsNil(location=builtin()),
        pi(nothing(), var("TYPE", implicitsNil(location=builtin()), location=builtin()),
                      var("TYPE", implicitsNil(location=builtin()), location=builtin()),
                      location=builtin())))
    , pair("nil", sig(implicitsCons("T", var("TYPE", implicitsNil(location=builtin()), location=builtin()),
                      implicitsNil(location=builtin()), location=builtin()),
        app(var("List", implicitsNil(location=builtin()), location=builtin()),
            var("T", implicitsNil(location=builtin()), location=builtin()),
            location=builtin())))
    , pair("cons", sig(implicitsCons("T", var("TYPE", implicitsNil(location=builtin()), location=builtin()),
                       implicitsNil(location=builtin()), location=builtin()),
        pi(nothing(), var("T", implicitsNil(location=builtin()), location=builtin()),
                      pi(nothing(), app(var("List", implicitsNil(location=builtin()), location=builtin()),
                                        var("T", implicitsNil(location=builtin()), location=builtin()),
                                        location=builtin()),
                                    app(var("List", implicitsNil(location=builtin()), location=builtin()),
                                        var("T", implicitsNil(location=builtin()), location=builtin()),
                                        location=builtin()),
                                    location=builtin()),
                      location=builtin())))
    ];
  local implicitDefaultEnv :: [Pair<String Maybe<Decorated implicit:Signature>>] = foldl(
    \l::[Pair<String Maybe<Decorated implicit:Signature>>] p::Pair<String Signature> ->
      pair(p.fst, just(decorate p.snd with {
        env = l;
      })) :: l,
    [], defaultEnv);
  local spinedDefaultEnv :: [Pair<String Maybe<unification:Expr>>] = map(
    \p::Pair<String Maybe<Decorated implicit:Signature>> ->
      case p.snd of
      | just(s) -> pair(p.fst, just(s.elaboratedExpr))
      | nothing() -> pair(p.fst, nothing())
      end,
    implicitDefaultEnv);

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
        cst :: Root_c = result.parseTree;
        if !null(cst.errors) then {
          printM(messagesToString(cst.errors) ++ "\n");
          return 1;
        } else {
          printM("cst pp:\n" ++ show(80, cst.pp));
          astPreElaboration :: Decorated implicit:Decls = decorate cst.ast with {
            env = implicitDefaultEnv;
          };
          if !null(astPreElaboration.errors) then {
            printM(messagesToString(astPreElaboration.errors) ++ "\n");
            return 1;
          } else {
            printM("\nast pp (pre-elaboration):\n" ++ show(80, astPreElaboration.pp));
            astPreUnification :: Decorated unification:Decls =
              decorate astPreElaboration.elaboratedDecls with {
                unification:inhTyEnv = spinedDefaultEnv;
              };
            printM("ast pp (pre-unification):\n" ++ show(80, astPreUnification.pp));
            if !null(astPreUnification.errors) then {
              printM(messagesToString(astPreUnification.errors) ++ "\n");
              return 1;
            } else {
              printM("ast constraints:\n" ++ show(80, ppConcat(map(
                \c::unification:Constraint -> cat(c.pp, line()),
                astPreUnification.constraints))));
              astPostUnification :: Decorated unification:Decls =
                decorate astPreUnification.unified with {
                  unification:inhTyEnv = spinedDefaultEnv;
                };
              if !null(astPostUnification.errors) then {
                printM(messagesToString(astPostUnification.errors) ++ "\n");
                return 1;
              } else {
                printM("\nast pp (post-unification):\n" ++ show(80, astPostUnification.pp));
                if astPostUnification.hasVars then {
                  printM("error: post-unification ast has unsolved variables!\n");
                  return 1;
                } else {
                  -- TODO
                  printM("TODO: FINISH COMPILING!\n");
                  return 0;
                }
              }
            }
          }
        }
      }
    }
  }, ioIn);
}
