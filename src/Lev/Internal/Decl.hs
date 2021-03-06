{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Lev.Internal.Decl where

import Control.Lens         (makeFields, (^.))
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable        (find)
import Data.String          (IsString)
import GHC.Generics         (Generic)
import Lev.Internal.Expr

import qualified Data.Map as Map

data Definition a = Definition
  { definitionVarName :: a
  , definitionValue   :: Term a
  } deriving (Eq, Show, Read, Functor, Foldable, Traversable, Generic)
makeFields ''Definition

data Signature a = Signature
  { signatureVarName :: a
  , signatureType_   :: Term a
  } deriving (Eq, Show, Read, Functor, Foldable, Traversable, Generic)
makeFields ''Signature



-- | A program is a list of definitions and signatures. The definitions and
-- signatures are expected to match up -- i.e., to refer to the same variable.
newtype Program a = Program { getProg :: [(Definition a, Signature a)] }
  deriving (Eq, Show, Read, Functor, Foldable, Traversable, Generic)

------------------------------------------------------------------------------
-- * Type Checking
------------------------------------------------------------------------------

-- | Checks that a definition has correct type, and adds it to the environment
-- if so (throwing an error otherwise).
typeCheckDecl :: Unbound a => Definition a -> Signature a -> Environment a ()
typeCheckDecl def typ = do
  when (def ^. varName /= typ ^. varName) $
    throwError "Type signature variable name does not match value name"
  withEnvironment $ \e -> checkType e (typ ^. type_) Type
  -- We don't allow recursion yet, so we don't need to extend the context or
  -- add to the environment until we're done.
  withEnvironment $ \e -> checkType e (def ^. value) (typ ^. type_)
  addTermType (def ^. varName) (typ ^. type_)

withEnvironment :: (Context a -> Either String v) -> Environment a v
withEnvironment fn = do
  e <- get
  case fn (Context e) of
    Left err  -> throwError err
    Right v -> return v

-- | Typecheck the entire program.
typeCheckProgram :: Unbound a => Program a -> Environment a ()
typeCheckProgram prog
  = foldM (\() (def, decl) -> typeCheckDecl def decl) () (getProg prog)

------------------------------------------------------------------------------
-- * Evaluation
------------------------------------------------------------------------------

evaluateProgram :: forall a. (IsString a, Unbound a)
  => Program a -> Environment a (Term a)
evaluateProgram prog = do
  typeCheckProgram prog
  nf <$> getMain
  where
    getMain :: Environment a (Term a)
    getMain = case find (\a -> a ^. varName == "main") $ fmap fst (getProg prog) of
      Nothing -> throwError "Can't evaluate program with no 'main'!"
      Just v  -> return $ v ^. value

------------------------------------------------------------------------------
-- * Environment
------------------------------------------------------------------------------

newtype Environment v a = Environment
  { getEnv :: ExceptT String (State (Map.Map v (Term v))) a }
  deriving ( Functor, Applicative, Monad, MonadError String
           , MonadState (Map.Map v (Term v)))

addTermType :: Unbound v => v -> Term v -> Environment v ()
addTermType name typ = modify (Map.insert name typ)

runEnvironment :: Environment v a -> Either String a
runEnvironment (Environment e) = evalState (runExceptT e) Map.empty

{-
-- | Resolve all free variables by looking them up in the environment.
-- Currently fails by burning.
resolve :: (Ord a, Show a) => Term a -> Environment a (Term Void)
resolve x = do
  env <- get
  let force y = case closed y of
        Nothing -> error "Should not happen"
        Just v  -> v
  let resolved = instantiate (\y -> case Map.lookup y env of
        Nothing -> error "Could not find variable"
        Just z  -> force z) $ abstract Just x
  return $ force resolved
  -}
