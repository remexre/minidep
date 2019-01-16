import Control.Monad.Identity
import System

%default total

----------------------------------------------------------------------------------------------------

interface Monad m => MonadIO (m : Type -> Type) where
  liftIO : IO a -> m a

MonadIO IO where liftIO = id

enumerate : List a -> List (Nat, a)
enumerate l = helper Z l where
  helper _ [] = []
  helper n (h::t) = (n, h) :: helper (S n) t

----------------------------------------------------------------------------------------------------

data Expr : Type where
  App : Expr -> Expr -> Expr
  Arr : String -> Expr -> Expr -> Expr
  Ty : Expr
  Var : String -> Expr

fv : Expr -> List String
fv (App l r) = fv l ++ fv r
fv (Arr n l r) = fv l ++ filter (\x => x /= n) (fv r)
fv Ty = []
fv (Var v) = [v]

Show Expr where
  show (App f x) = show f ++ " (" ++ show x ++ ")"
  show (Arr n l r) =
    if n `elem` fv r then
      "(" ++ n ++ " : " ++ show l ++ ") -> " ++ show r
    else
      "(" ++ show l ++ ") -> " ++ show r
  show Ty = "*"
  show (Var n) = n

apps : Expr -> List Expr -> Expr
apps f []     = f
apps f (h::t) = apps (App f h) t

arrs : List (String, Expr) -> Expr -> Expr
arrs []          x = x
arrs ((n, v)::t) x = Arr n v (arrs t x)

flattenApp : Expr -> (Expr, List Expr)
flattenApp (App f x) with (flattenApp f)
  flattenApp (App f x) | (f', xs) = (f', xs ++ [x])
flattenApp a = (a, [])

isVar : String -> Expr -> Bool
isVar s (Var t) = s == t
isVar _ _ = False

showIndented : Nat -> Expr -> String
showIndented w (Arr n l r) =
  let end = "\n" ++ concat (replicate w " ") ++ showIndented w r in
  if n `elem` fv r then
    "(" ++ n ++ " : " ++ show l ++ ") ->" ++ end
  else
    "(" ++ show l ++ ") ->" ++ end
showIndented _ e = show e

record Ctor where
  constructor MkCtor
  ctorName : String
  ctorArgs : List (String, Expr)
  ctorTyArgs : List Expr

record Data where
  constructor MkData
  dataName : String
  dataArgs : List (Maybe String, Expr)
  dataCtors : List Ctor

----------------------------------------------------------------------------------------------------

dataVoid : Data
dataVoid = MkData "Void" [] []

elimVoid : Expr
elimVoid = arrs args (App (Var "m") (Var "v")) where
  args = [ ("m", Arr "_" (Var "Void") Ty)
         , ("v", Var "Void")
         ]

dataUnit : Data
dataUnit = MkData "Unit" [] [MkCtor "unit" [] []]

dataBool : Data
dataBool = MkData "Bool" [] [MkCtor "false" [] [], MkCtor "true" [] []]

dataNat : Data
dataNat = MkData "Nat" [] ctors where
  ctors = [ MkCtor "z" []                 []
          , MkCtor "s" [("n", Var "Nat")] []
          ]

dataList : Data
dataList = MkData "List" [(Just "T", Ty)] ctors where
  ctors = [ MkCtor "nil"  [] [Var "T"]
          , MkCtor "cons" [("h", Var "T"), ("t", App (Var "List") (Var "T"))] [Var "T"]
          ]

dataVect : Data
dataVect = MkData "Vect" [(Just "T", Ty), (Nothing, Var "Nat")] ctors where
  ctors = [ MkCtor "nil"  [] [Var "T", Var "z"]
          , MkCtor "cons" [ ("n", Var "Nat")
                          , ("h", Var "T")
                          , ("t", App (App (Var "Vect") (Var "T")) (Var "n"))
                          ] [Var "T", App (Var "s") (Var "n")]
          ]

----------------------------------------------------------------------------------------------------

data GensymT : (m : Type -> Type) -> (a : Type) -> Type where
  MkGensymT : {m : Type -> Type} -> {a : Type} -> (Nat -> m (a, Nat)) -> GensymT m a

gensym : Applicative m => GensymT m String
gensym = MkGensymT (\n => pure ("G#" ++ show n, n + 1))

gensymAs : Applicative m => String -> GensymT m String
gensymAs s = MkGensymT (\n => pure ("G#" ++ show n ++ "#" ++ s, n + 1))

runGensymT : GensymT m a -> Nat -> m (a, Nat)
runGensymT (MkGensymT f) = f

Functor m => Functor (GensymT m) where
  map f x = MkGensymT (map f' . runGensymT x) where
    f' (x', n') = (f x', n')

Monad m => Applicative (GensymT m) where
  pure x = MkGensymT (\n => pure (x, n))
  (<*>) f x = MkGensymT ap where
    ap n = do
      (f', n' ) <- runGensymT f n
      (x', n'') <- runGensymT x n'
      pure (f' x', n'')

Monad m => Monad (GensymT m) where
  (>>=) x f = MkGensymT (\n => runGensymT x n >>= \(x', n') => runGensymT (f x') n')

lift : Functor m => m a -> GensymT m a
lift x = MkGensymT (\n => map (f n) x) where
  f n x = (x, n)

MonadIO m => MonadIO (GensymT m) where liftIO = lift . liftIO

Gensym : Type -> Type
Gensym = GensymT Identity

runGensym : Gensym a -> Nat -> (a, Nat)
runGensym m = runIdentity . runGensymT m

GensymIO : Type -> Type
GensymIO = GensymT IO

----------------------------------------------------------------------------------------------------

tyArgsToMotiveInvocation : String -> List Expr -> List ((String, Expr), Bool) -> String -> Expr
tyArgsToMotiveInvocation motiveName tyArgs params ctorName = ret where
  invoc : Expr
  invoc = apps (Var ctorName) tyArgs
  uArgs : List Expr
  uArgs = map snd . filter (snd . fst) $ zip params tyArgs
  ret : Expr
  ret = apps (Var motiveName) (uArgs ++ [invoc])

elimCtor : Monad m => String -> String -> List ((String, Expr), Bool) -> Ctor -> GensymT m Expr
elimCtor tyName motiveName params ctor = do
  let args = ctorArgs ctor
  let argNames = map (Var . fst) args
  -- Find the inductive arguments, i.e. those which are applications of tyName.
  let ind' = filter (isVar tyName . fst . flattenApp . snd) $ args
  -- TODO: Add the relevant unnamed args.
  let ind' = map (apps (Var motiveName) . (::[]) . Var . fst) ind'
  ind <- traverse (\expr => map (\name => (name, expr)) gensym) ind'
  -- Assign arguments to the motive (for the return value of the constructor case).
  let unnamedArgs = map snd . filter (not . snd . fst) $ zip params (ctorTyArgs ctor)
  let appArgs = unnamedArgs ++ [apps (Var $ ctorName ctor) argNames]
  -- Assemble the type.
  let invoc = tyArgsToMotiveInvocation motiveName (map (Var . fst) $ ctorArgs ctor) params (ctorName ctor)
  pure $ arrs (ctorArgs ctor ++ ind) invoc
  -- pure $ arrs (ctorArgs ctor ++ ind) $ apps (Var motiveName) appArgs

nameDataParams : Monad m => Data -> GensymT m (List ((String, Expr), Bool))
nameDataParams = traverse f . dataArgs where
  f (Just name, ty) = pure ((name, ty), True)
  f (Nothing, ty) = map (\name => ((name, ty), False)) gensym

elim : Monad m => Data -> GensymT m Expr
elim datatype = do
  let name = dataName datatype
  let name' = Var name
  -- Extract the parameters.
  params <- nameDataParams datatype
  let paramNames = with Basics map (fst . fst) params
  let paramNames' = map Var paramNames
  let named = map fst . filter snd $ params
  let unnamed = map fst . filter (not . snd) $ params
  -- Create the type of the motive.
  motiveName <- gensym
  motiveTy <- pure $ arrs (unnamed ++ [(!gensym, apps name' paramNames')]) Ty
  let motive = [(motiveName, motiveTy)]
  -- Create the case arguments.
  cases' <- traverse (elimCtor name motiveName params) (dataCtors datatype)
  let cases = zip (map ctorName $ dataCtors datatype) cases'
  -- Create the value.
  valueName <- gensym
  let valueTy = apps name' paramNames'
  let value = [(valueName, valueTy)]
  -- Assemble the type.
  pure $ arrs (concat [named, motive, cases, unnamed, value]) (App (Var motiveName) (Var valueName))

{-
assertEq : MonadIO m => String -> Expr -> Expr -> m ()
assertEq label l r = if l == r then pure () else liftIO fail where
  fail = do
    putStrLn ("Assertion " ++ show label ++ " failed:")
    putStrLn ("   left: " ++ showIndented l)
    putStrLn ("  right: " ++ showIndented r)
    putStrLn ""
-}

elimTests : IO ()
elimTests = map fst . flip runGensymT 0 . the (GensymIO _) $ do
  liftIO $ putStr "Void: "
  elimVoid' <- elim dataVoid
  liftIO $ putStrLn $ showIndented 6 elimVoid'

  liftIO $ putStr "Unit: "
  elimUnit' <- elim dataUnit
  liftIO $ putStrLn $ showIndented 6 elimUnit'

  liftIO $ putStr "Bool: "
  elimBool' <- elim dataBool
  liftIO $ putStrLn $ showIndented 6 elimBool'

  liftIO $ putStr " Nat: "
  elimNat' <- elim dataNat
  liftIO $ putStrLn $ showIndented 6 elimNat'

  liftIO $ putStr "List: "
  elimList' <- elim dataList
  liftIO $ putStrLn $ showIndented 6 elimList'

  liftIO $ putStr "Vect: "
  elimVect' <- elim dataVect
  liftIO $ putStrLn $ showIndented 6 elimVect'
