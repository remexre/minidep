grammar edu:umn:cs:melt:minidep:util;

import core:monad;
import silver:langutil;

function bindErrIO
IOMonad<Either<[Message] b>> ::= m::IOMonad<Either<[Message] a>> fn::(IOMonad<Either<[Message] b>> ::= a)
{
  return bindIO(m, \e::Either<[Message] a> ->
    case e of
    | left(errs) -> returnIO(left(errs))
    | right(x) -> fn(x)
    end);
}

function returnErrIO
IOMonad<Either<[Message] a>> ::= x::a
{
  return returnIO(right(x));
}

function evalErrIO
IOVal<a> ::= m::IOMonad<Either<[Message] a>> ioIn::IO
{
  return evalIO(do(bindIO, returnIO) {
    result :: Either<[Message] a> <- m;
    case result of
    | left(errs) -> do(bindIO, returnIO) {
        printM(messagesToString(errs) ++ "\n");
        exitM(1);
      }
    | right(ret) -> returnIO(ret)
    end;
  }, ioIn);
}

function liftErr
IOMonad<Either<[Message] a>> ::= errs::Either<[Message] a>
{
  return returnIO(errs);
}

function liftIO
IOMonad<Either<[Message] a>> ::= m::IOMonad<a>
{
  return bindIO(m, \x::a -> returnIO(right(x)));
}

function throw
IOMonad<Either<[Message] a>> ::= errs::[Message]
{
  return returnIO(left(errs));
}

function throwIfAny
IOMonad<Either<[Message] Unit>> ::= errs::[Message]
{
  return if null(errs)
         then returnErrIO(unit())
         else throw(errs);
}

function throwOne
IOMonad<Either<[Message] a>> ::= errs::Message
{
  return throw([errs]);
}

function mustParse
IOMonad<Either<[Message] a>> ::= parse::(ParseResult<a> ::= String String) path::String
{
  return do (bindErrIO, returnErrIO) {
    src :: String <- liftIO(readFileM(path));
    result :: ParseResult<a> = parse(src, path);
    if !result.parseSuccess then {
      e :: Message = case result.parseError of
      | syntaxError(msg, l, _, _) -> err(l, msg)
      | unknownParseError(msg, file) -> err(txtLoc(file), msg)
      end;
      throw([e]);
    } else {
      return result.parseTree;
    }
  };
}
