{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Lev.Internal.Expr where

import Bound
import Control.Monad.Except
import Control.Monad.State
import Data.Deriving        (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import Data.String
import Data.Void
import GHC.Generics         (Generic)

import qualified Data.Map as Map

-- We use Normalization by Evaluation (NbE) to normalize terms during
-- type-checking. This involves transforming the deep embedding of syntactic
-- terms into its denotation or semantics, which is a shallow embedding (this
-- process is called 'reflection'). Then we *reify* the semantic term back into
-- a syntactic one.

------------------------------------------------------------------------------
-- * Syntactic (Terms)
------------------------------------------------------------------------------

-- We use bidirectional typechecking; however, since it turns out to be hard or
-- impossible to use 'bound' with mutually recursive datatypes, we coalesce
-- inferrable and checkable terms into a single datatype. This in turn means
-- some terms are representable that are invalid.

------------------------------------------------------------------------------
-- ** Types

-- | Inferrable or checkable terms.
data Term a

  -- Inferrable
  = Annotation (Term a) (Term a)
  | Type
  | Application (Term a) (Term a)
  | Var a
  | Pi (Term a) (Scope () Term a) -- (pi (x : X) x)

  -- Checkable
  | Lambda (Scope () Term a)
  deriving (Functor, Foldable, Traversable, Generic)

makeBound   ''Term
deriveEq1   ''Term
deriveOrd1  ''Term
deriveShow1 ''Term
deriveRead1 ''Term
instance Show a => Show (Term a) where showsPrec = showsPrec1
instance Read a => Read (Term a) where readsPrec = readsPrec1
instance Eq   a => Eq   (Term a) where (==)      = eq1
instance Ord  a => Ord  (Term a) where compare   = compare1

instance IsString a => IsString (Term a) where
  fromString = Var . fromString


------------------------------------------------------------------------------
-- ** Helpers

pi :: Eq a => a -> Term a -> Term a -> Term a
pi var typ term = Pi typ $ abstract1 var term

lambda :: Eq a => a -> Term a -> Term a
lambda var term = Lambda $ abstract1 var term

variable :: a -> Term a
variable = Var

($$) :: Term a -> Term a -> Term a
($$) = Application

(-:) :: Term a -> Term a -> Term a
(-:) = Annotation


------------------------------------------------------------------------------
-- * Environment
------------------------------------------------------------------------------

newtype Environment v a = Environment
  { getEnv :: ExceptT String (State (Map.Map v (Term Void))) a }
  deriving ( Functor, Applicative, Monad, MonadError String
           , MonadState (Map.Map v (Term Void)))

------------------------------------------------------------------------------
-- * Context
------------------------------------------------------------------------------

newtype Context v = Context { getCtx :: Map.Map v (Term v) }
  deriving (Eq, Show, Ord, Generic)

emptyCtx :: Context v
emptyCtx = Context $ Map.empty

lookupCtx :: Ord v => v -> Context v -> Maybe (Term v)
lookupCtx v (Context ctx) = Map.lookup v ctx

extendCtx :: Ord v => v -> Term v -> Context v -> Context v
extendCtx var typ (Context ctx) = Context $ Map.insert var typ ctx


------------------------------------------------------------------------------
-- * Evaluation
------------------------------------------------------------------------------

nf :: Term a -> Term a
nf (Annotation e y) = Annotation (nf e) (nf y)
nf Type = Type
nf (Application fn val) = case nf fn of
  Lambda x -> nf (instantiate1 val x)
  fn'      -> Application fn' (nf val)
nf (Var x) = Var x
nf (Lambda x) = Lambda $ toScope $ nf $ fromScope x
nf (Pi typ x) = Pi (nf typ) (toScope $ nf $ fromScope x)

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

------------------------------------------------------------------------------
-- * Type checking
------------------------------------------------------------------------------

inferType :: Ord v => Context v -> Term v -> Either String (Term v)
inferType ctx t = inferType' ctx (Right <$> t)

inferType' :: Ord v => Context v -> Term (Either (Term v) v) -> Either String (Term v)
inferType' ctx term = case term of
  Annotation e an -> do
    checkType' ctx an Type
    checkType' ctx e  an
    return $ fromRight <$> an
  Application fn val -> do
    fnType <- inferType' ctx fn
    case fnType of
      Pi arg binding -> do
        checkType ctx arg Type
        return $ fromRight <$> instantiate1 val (Right <$> binding)
      _ -> throwError "Application of non-function type"
  Var x -> case lookupCtx (fromRight x) ctx of
    Nothing -> throwError "Unknown identifier"
    Just v  -> return v
  Pi typ binding -> do
    checkType' ctx typ Type
    checkType' ctx (instantiate1 Type binding) Type
    return Type
  Type -> return Type
  _ -> throwError "Can't infer type"

checkType :: Ord v => Context v -> Term v -> Term v -> Either String ()
checkType ctx term typ = checkType' ctx (Right <$> term) (Right <$> typ)

checkType' :: Ord v => Context v -> Term (Either (Term v) v)
  -> Term (Either (Term v) v) -> Either String ()
checkType' ctx (Lambda e) (Pi t b) = do
  checkType' ctx (instantiate1 t e)
                 (instantiate1 t b)
checkType' ctx x expectedType = do
  actualType <- inferType' ctx x
  when ((Right <$> actualType) /= expectedType) $ throwError "Type mismatch"


fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Not right"
{-
------------------------------------------------------------------------------
-- Bibliography

[MiniTT] A simple type-theoretic language: Mini-TT (Coquand et al)
[PracLev] The Practical Guide to Levitation (Al-Sibahi)
[GentleLev] The Gentle Art of Levitation (Chapman et al)
-}
