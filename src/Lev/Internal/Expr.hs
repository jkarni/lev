{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell            #-}
module Lev.Internal.Expr where

import Bound
import Control.Monad.Except
import Data.Deriving        (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import Data.Monoid
import Data.String
import GHC.Generics         (Generic)

import qualified Data.Map  as Map
import qualified Data.Text as T

------------------------------------------------------------------------------
-- * Unbound
------------------------------------------------------------------------------
class (Ord a, Show a) => Unbound a where
  unbound :: a
instance Unbound String where unbound = "_"
instance Unbound T.Text where unbound = "_"
instance Unbound v => Unbound (Either (Term v) v)  where unbound = Right unbound


------------------------------------------------------------------------------
-- * Terms
------------------------------------------------------------------------------

-- | Inferrable or checkable terms.
--
-- 'bound' doesn't allow mutually recursive types, so all types (inferrable,
-- checkable, description) are folded in.
data Term a

  -- Inferrable
  = Annotation (Term a) (Term a)      -- (: val typ)
  | Type                              -- Type
  | Application (Term a) (Term a)     -- (fn val)
  | Var a                             -- x
  | Pi (Term a) (Scope () Term a)     -- (Pi X x x)
  | Sigma (Term a) (Scope () Term a)  -- (Sigma X x x)
  | UnitType                          -- Unit
  | UnitValue                         -- ()
  | Tag T.Text                        -- #Zero
  | TagType                           -- Tag

  --- Description
  | Description (Term a)
  | EndDesc (Term a)
  | RecDesc (Term a) (Term a)
  | ArgDesc (Term a) (Term a)

  -- Checkable
  | Pair (Term a) (Term a)            -- '(x y)
  | Lambda (Scope () Term a)          -- (lam x x)
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

fnType :: Unbound a => Term a -> Term a -> Term a
fnType arg term = Pi arg $ abstract1 unbound term

sigma :: Eq a => a -> Term a -> Term a -> Term a
sigma var typ term = Sigma typ $ abstract1 var term

lambda :: Eq a => a -> Term a -> Term a
lambda var term = Lambda $ abstract1 var term

pair :: Term a -> Term a -> Term a
pair = Pair

variable :: a -> Term a
variable = Var

($$) :: Term a -> Term a -> Term a
($$) = Application

(-:) :: Term a -> Term a -> Term a
(-:) = Annotation



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
nf (Sigma typ x) = Sigma (nf typ) (toScope $ nf $ fromScope x)
nf (Pair a b) = Pair (nf a) (nf b)
nf UnitType = UnitType
nf UnitValue = UnitValue
nf TagType = TagType
nf (Description x) = Description $ nf x
nf (EndDesc x) = EndDesc $ nf x
nf (RecDesc x y) = RecDesc (nf x) (nf y)
nf (ArgDesc x y) = ArgDesc (nf x) (nf y)
nf (Tag x) = Tag x


------------------------------------------------------------------------------
-- * Type checking
------------------------------------------------------------------------------
-- We use "Either (Term v) v" for variable type, indicating that it is either a
-- name or a type. As we move under binders, we transform bound names into
-- types.

inferType :: Unbound v => Context v -> Term v -> Either String (Term v)
inferType ctx t = inferType' ctx (Right <$> t)

inferType' :: Unbound v => Context v -> Term (Either (Term v) v) -> Either String (Term v)
inferType' ctx term = case term of
  Annotation e an -> do
    checkType' ctx an Type
    checkType' ctx e  an
    return $ fromRight <$> an
  Application fn val -> do
    fnType' <- inferType' ctx fn
    case fnType' of
      Pi arg binding -> do
        checkType' ctx (Right <$> arg) Type
        return $ fromRight <$> instantiate1 val (Right <$> binding)
      _ -> throwError "Application of non-function type"
  Var (Left e) -> do
    return e -- local variable
  Var (Right x) -> case lookupCtx x ctx of  -- global variable
    Nothing -> throwError "Unknown identifier"
    Just v  -> return v
  Pi typ binding -> do
    checkType' ctx typ Type
    checkType' ctx (instantiate1 Type binding) Type
    return Type
  Sigma typ binding -> do
    checkType' ctx typ Type
    checkType' ctx (instantiate1 Type binding) Type
    return Type
  Type      -> return Type
  UnitType  -> return Type
  UnitValue -> return UnitType
  Tag _     -> return TagType
  TagType   -> return Type
  Description x -> do
    checkType' ctx x Type
    return Type

  Lambda _    -> throwError "Can't infer type for lambdas"
  Pair _ _    -> throwError "Can't infer type for pairs"
  EndDesc _   -> throwError "Can't infer type for descriptions"
  RecDesc _ _ -> throwError "Can't infer type for descriptions"
  ArgDesc _ _ -> throwError "Can't infer type for descriptions"

checkType :: Unbound v => Context v -> Term v -> Term v -> Either String ()
checkType ctx term typ = checkType' ctx (Right <$> term) (Right <$> typ)

checkType' :: Unbound v => Context v -> Term (Either (Term v) v)
  -> Term (Either (Term v) v) -> Either String ()
checkType' ctx (Lambda e) (Pi t b) = do
  -- (: (lambda x x) (pi Int t Int))
  -- In the lambda we instantiate with 'Left t', meaning the *type* of the
  -- variable must be 't'. In pi, we instantiate with 'Right t', meaning the
  -- variable itself must
  above <- inferType' ctx t
  checkType' ctx (instantiate1 (toTyped t) e)
                 (instantiate1 (Var $ Left above) b)
checkType' ctx (Pair a b) (Sigma t t') = do
  checkType' ctx a t
  checkType' ctx b (instantiate1 t t')
checkType' ctx (EndDesc x) (Description typ) = checkType' ctx x typ
checkType' ctx (RecDesc a b) (Description typ) = do
  checkType' ctx a typ
  checkType' ctx b (Description typ)
checkType' ctx (ArgDesc a b) d@(Description _) = do
  checkType' ctx a Type
  checkType' ctx b (fnType a d)
checkType' ctx x (Var (Left expectedType)) = do
  actualType <- inferType' ctx x
  when (actualType /= expectedType) $
    throwError $ "Type mismatch. Expected:\n\t" <> show expectedType
              <> "Saw:\n\t" <> show actualType
checkType' ctx x expectedType = do
  actualType <- inferType' ctx x
  when ((Right <$> actualType) /= expectedType) $
    throwError $ "Type mismatch. Expected:\n\t" <> show expectedType
              <> "Saw:\n\t" <> show actualType

toTyped :: Term (Either (Term v) v) -> Term (Either (Term v) v)
toTyped v = Var . Left $ fromRight <$> v

fromRight :: Either a b -> b
fromRight (Right x) = x
fromRight _ = error "Not right"
{-
------------------------------------------------------------------------------
-- Bibliography

[MiniTT] A simple type-theoretic language: Mini-TT (Coquand et al)
[PracLev] The Practical Guide to Levitation (Al-Sibahi)
[GentleLev] The Gentle Art of Levitation (Chapman et al)
[GenElim] Generic Constructors and Eliminators from Descriptions (Diehl, Sheard)
-}
