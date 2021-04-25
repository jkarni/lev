{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lev.Internal.Decl where

import Control.Lens ((^.), makeFields)
import Control.Monad.Except
import Control.Monad.State
import Data.Foldable (find)
import qualified Data.Map as Map
import Data.String (IsString)
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lev.Internal.Expr
import Prettyprinter ((<+>), Pretty (..), indent, line)

data Definition b a
  = Definition
      { definitionVarName :: a,
        definitionValue :: Term b a
      }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

makeFields ''Definition

data Signature b a
  = Signature
      { signatureVarName :: a,
        signatureType_ :: Term b a
      }
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

makeFields ''Signature

-- | A program is a list of definitions and signatures. The definitions and
-- signatures are expected to match up -- i.e., to refer to the same variable.
newtype Program b a = Program {getProg :: [(Definition b a, Signature b a)]}
  deriving stock (Eq, Show, Functor, Foldable, Traversable, Generic)

------------------------------------------------------------------------------

-- * Type Checking
------------------------------------------------------------------------------

-- | Checks that a definition has correct type, and adds it to the environment
-- if so (throwing an error otherwise).
typeCheckDecl ::
  (Show a, Monoid b, Unbound a) =>
  Definition b a ->
  Signature b a ->
  Environment b a ()
typeCheckDecl def typ = do
  when (def ^. varName /= typ ^. varName)
    $ throwError
    $ SignatureAndDefinitionMismatch
      { _signature = T.pack . show $ typ ^. varName,
        _definition = T.pack . show $ def ^. varName,
        _location = error "notimpl"
      }
  withEnvironment $ \e -> checkType e (typ ^. type_) (Type mempty)
  -- We don't allow recursion yet, so we don't need to extend the context or
  -- add to the environment until we're done.
  withEnvironment $ \e -> checkType e (def ^. value) (typ ^. type_)
  addTermType (def ^. varName) (typ ^. type_)

withEnvironment :: (Context b a -> M b v) -> Environment b a v
withEnvironment fn = do
  e <- get
  case runM $ fn (Context e) of
    Left (err, bs) -> throwError $ TypeErr bs err
    Right v -> return v

-- | Typecheck the entire program.
typeCheckProgram :: (Monoid b, Unbound a) => Program b a -> Environment b a ()
typeCheckProgram prog =
  foldM (\() (def, decl) -> typeCheckDecl def decl) () (getProg prog)

------------------------------------------------------------------------------

-- * Evaluation
------------------------------------------------------------------------------

evaluateProgram ::
  forall a b.
  (IsString a, Monoid b, Unbound a) =>
  Program b a ->
  Environment b a (Term b a)
evaluateProgram prog = do
  typeCheckProgram prog
  nf <$> getMain
  where
    getMain :: Environment b a (Term b a)
    getMain = case find (\a -> a ^. varName == "main") $ fmap fst (getProg prog) of
      Nothing -> throwError MissingMain
      Just v -> return $ v ^. value

------------------------------------------------------------------------------

-- * Environment
------------------------------------------------------------------------------

data Error b
  = TypeErr {_locations :: [b], _typeError :: TypeError}
  | SignatureAndDefinitionMismatch
      {_location :: b, _signature :: T.Text, _definition :: T.Text}
  | MissingMain
  deriving stock (Eq, Show, Read, Generic, Functor)

instance Pretty (Error b) where
  pretty x = case x of
    TypeErr {..} -> pretty _typeError
    MissingMain -> "Missing 'main'"
    SignatureAndDefinitionMismatch {..} ->
      "Signature and definition names don't match."
        <> line
        <> ( indent 2 $
               "Signature:" <+> pretty _signature <> line
                 <> "Definition:" <+> pretty _definition
           )

newtype Environment b v a
  = Environment
      {getEnv :: ExceptT (Error b) (State (Map.Map v (Term b v))) a}
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadError (Error b),
      MonadState (Map.Map v (Term b v))
    )

addTermType :: Unbound v => v -> Term b v -> Environment b v ()
addTermType name typ = modify (Map.insert name typ)

runEnvironment :: Environment b v a -> Either (Error b) a
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
