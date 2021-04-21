{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lev.Internal.Expr where

import Bound
import qualified Bound.Name as BN
import Control.Monad.Except
import Data.Deriving (deriveEq1, deriveOrd1, deriveRead1, deriveShow1)
import Data.Functor.Classes
import qualified Data.Map as Map
import Data.String
import Data.String.Conversions (ConvertibleStrings, convertString)
import qualified Data.Text as T
import GHC.Generics (Generic)
import GHC.Stack (HasCallStack)
import Prettyprinter ((<+>), Pretty (..), indent, line, squotes)

------------------------------------------------------------------------------

-- * Unbound
------------------------------------------------------------------------------

class (Ord a, Show a) => Unbound a where
  unbound :: a

instance Unbound String where unbound = "_"

instance Unbound T.Text where unbound = "_"

instance Unbound v => Unbound (Either (Term v) v) where unbound = Right unbound

------------------------------------------------------------------------------

-- * Terms
------------------------------------------------------------------------------

-- See https://github.com/ekmett/bound/issues/74 for the motivation for this.
data One = One
  deriving stock (Eq, Show, Ord, Generic, Read)

type Name = BN.Name T.Text One

-- | Inferrable or checkable terms.
--
-- 'bound' doesn't allow mutually recursive types, so all types (inferrable,
-- checkable, description) are folded in.
data Term a
  = -- Inferrable
    Annotation (Term a) (Term a) -- (: val typ)
  | Type -- Type
  | Application (Term a) (Term a) -- (fn val)
  | Var a -- x
  | Pi (Term a) Name (Scope Name Term a) -- (pi X x x)
  | Sigma (Term a) Name (Scope Name Term a) -- (Sigma X x x)
  | UnitType -- Unit
  | UnitValue -- ()
  | Tag T.Text -- #Zero
  | TagType -- Tag
    -- Macros
  | Unquote (Term a)
  | -- Description
    Description (Term a)
  | EndDesc (Term a)
  | RecDesc (Term a) (Term a)
  | ArgDesc (Term a) (Term a)
  | -- Checkable
    Pair (Term a) (Term a) -- '(x y)
  | Lambda Name (Scope Name Term a) -- (lam x x)
  deriving (Functor, Foldable, Traversable, Generic)

makeBound ''Term

deriveEq1 ''Term

deriveOrd1 ''Term

deriveShow1 ''Term

deriveRead1 ''Term

instance Show a => Show (Term a) where showsPrec = showsPrec1

instance Read a => Read (Term a) where readsPrec = readsPrec1

instance Eq a => Eq (Term a) where (==) = eq1

instance Ord a => Ord (Term a) where compare = compare1

instance IsString a => IsString (Term a) where
  fromString = Var . fromString

------------------------------------------------------------------------------

-- ** Helpers

abstractT :: (Monad f, ConvertibleStrings a T.Text, Eq a) => a -> f a -> Scope Name f a
abstractT var = abstractName (== var)

abstractName ::
  (ConvertibleStrings a T.Text, Monad f) =>
  (a -> Bool) ->
  f a ->
  Scope Name f a
abstractName f t = Scope (liftM k t)
  where
    k a =
      if f a
        then B (BN.Name (convertString a) One)
        else F (return a)

pi :: (ConvertibleStrings a T.Text, Eq a) => a -> Term a -> Term a -> Term a
pi var typ term = Pi typ (mkName var) $ abstractT var term

fnType :: forall a. (Unbound a) => Term a -> Term a -> Term a
fnType arg term =
  let k a = F (return a)
   in Pi arg (BN.Name "_" One) $ Scope (liftM k term)

sigma :: (ConvertibleStrings a T.Text, Eq a) => a -> Term a -> Term a -> Term a
sigma var typ term = Sigma typ (mkName var) $ abstractT var term

lambda :: (ConvertibleStrings a T.Text, Eq a) => a -> Term a -> Term a
lambda var term = Lambda (mkName var) $ abstractT var term

mkName :: ConvertibleStrings a T.Text => a -> Name
mkName var = BN.Name (convertString var) One

pair :: Term a -> Term a -> Term a
pair = Pair

variable :: a -> Term a
variable = Var

($$) :: Term a -> Term a -> Term a
($$) = Application

(-:) :: Term a -> Term a -> Term a
(-:) = Annotation

unquote :: Term a -> Term a
unquote p@(Pair _ _) = foldl1 Application $ unfoldPair p
  where
    unfoldPair (Pair a b) = a : unfoldPair b
    unfoldPair UnitValue = []
    unfoldPair x = [x]
unquote x = x

------------------------------------------------------------------------------

-- * Context
------------------------------------------------------------------------------

newtype Context v = Context {getCtx :: Map.Map v (Term v)}
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

nf :: (HasCallStack) => Term a -> Term a
nf (Annotation e y) = Annotation (nf e) (nf y)
nf Type = Type
nf (Application fn val) = case nf fn of
  Lambda _ x -> nf (instantiate1 val x)
  fn' -> Application fn' (nf val)
nf (Var x) = Var x
nf (Lambda v x) = Lambda v $ toScope $ nf $ fromScope x
nf (Pi typ v x) = Pi (nf typ) v (toScope $ nf $ fromScope x)
nf (Sigma typ v x) = Sigma (nf typ) v (toScope $ nf $ fromScope x)
nf (Pair a b) = Pair (nf a) (nf b)
nf UnitType = UnitType
nf UnitValue = UnitValue
nf TagType = TagType
nf (Description x) = Description $ nf x
nf (EndDesc x) = EndDesc $ nf x
nf (RecDesc x y) = RecDesc (nf x) (nf y)
nf (ArgDesc x y) = ArgDesc (nf x) (nf y)
nf (Tag x) = Tag x
nf (Unquote x) = nf $ unquote x

------------------------------------------------------------------------------

-- * Type checking
------------------------------------------------------------------------------

data TypeError
  = CantInfer T.Text
  | TypeMismatch {_expected :: T.Text, _actual :: T.Text}
  | CantApply {_actual :: T.Text}
  | UnknownIdentifier T.Text
  deriving stock (Eq, Show, Read, Generic)

instance Pretty TypeError where
  pretty x = case x of
    CantInfer c -> "Can't infer a" <+> pretty c
    UnknownIdentifier i -> "Unknown identifier" <+> squotes (pretty i)
    CantApply actual ->
      "Can't apply a" <+> pretty actual
    TypeMismatch {..} ->
      "Type mismatch." <> line
        <> ( indent 2 $
               "Expected:" <+> pretty _expected <> line
                 <> "Got:" <+> pretty _actual
           )

newtype M a = M {runM :: Either TypeError a}
  deriving stock (Eq, Show, Read, Generic)
  deriving newtype (Functor, Applicative, Monad, MonadError TypeError)

-- We use "Either (Term v) v" for variable type, indicating that it is either a
-- name or a type. As we move under binders, we transform bound names into
-- types.

inferType :: (HasCallStack, Unbound v) => Context v -> Term v -> M (Term v)
inferType ctx t = inferType' ctx (Right <$> t)

inferType' :: (HasCallStack, Unbound v) => Context v -> Term (Either (Term v) v) -> M (Term v)
inferType' ctx term = case term of
  Annotation e an -> do
    checkType' ctx an Type
    checkType' ctx e an
    return $ fromRight <$> an
  Application fn val -> do
    fnType' <- inferType' ctx fn
    case fnType' of
      Pi arg _ binding -> do
        checkType' ctx (Right <$> arg) Type
        return $ fromRight <$> instantiate1 val (Right <$> binding)
      _ -> throwError $ CantApply $ T.pack $ show fnType'
  Var (Left e) -> do
    return e -- local variable
  Var (Right x) -> case lookupCtx x ctx of -- global variable
    Nothing -> throwError $ UnknownIdentifier $ T.pack $ show x
    Just v -> return v
  Pi typ _ binding -> do
    checkType' ctx typ Type
    checkType' ctx (instantiate1 Type binding) Type
    return Type
  Sigma typ _ binding -> do
    checkType' ctx typ Type
    checkType' ctx (instantiate1 Type binding) Type
    return Type
  Type -> return Type
  UnitType -> return Type
  UnitValue -> return UnitType
  Tag _ -> return TagType
  TagType -> return Type
  Description x -> do
    checkType' ctx x Type
    return Type
  Unquote (Pair x UnitValue) -> inferType' ctx x
  Unquote p -> do
    -- "Unquote" takes an n-tuple and treats it as an n-ary application. For
    -- example, if we have "Unquote '(fn x y)", the type is the same as just
    -- "(fn x y)".
    inferType' ctx $ unquote p
  Lambda _ _ -> throwError $ CantInfer "lambda"
  Pair _ _ -> throwError $ CantInfer "pair"
  EndDesc i -> do
    t <- inferType' ctx i
    return $ Description t
  RecDesc _ _ -> throwError $ CantInfer "description-rec"
  ArgDesc _ _ -> throwError $ CantInfer "description-arg"

checkType :: (HasCallStack, Unbound v) => Context v -> Term v -> Term v -> M ()
checkType ctx term typ = checkType' ctx (Right <$> term) (Right <$> typ)

checkType' ::
  (HasCallStack, Unbound v) =>
  Context v ->
  Term (Either (Term v) v) ->
  Term (Either (Term v) v) ->
  M ()
checkType' ctx (Lambda _ e) (Pi t _ b) = do
  -- (: (lambda x x) (pi Int t Int))
  -- In the lambda we instantiate with 'Left t', meaning the *type* of the
  -- variable must be 't'. In pi, we instantiate with 'Right t', meaning the
  -- variable itself must
  checkType'
    ctx
    (instantiate1 (toTyped t) e)
    (instantiate1 t b)
checkType' ctx (Pair a b) (Sigma t _ t') = do
  checkType' ctx a t
  checkType' ctx b (instantiate1 t t')
checkType' ctx (EndDesc x) (Description typ) = checkType' ctx x typ
checkType' ctx (RecDesc a b) (Description typ) = do
  checkType' ctx a typ
  checkType' ctx b (Description typ)
checkType' ctx (ArgDesc a b) d@(Description _) = do
  checkType' ctx a Type
  checkType' ctx b (fnType a d)
{-checkType' ctx (Unquote (Pair fn arg)) typ -> do-}
{--- "Unquote" takes an n-tuple and treats it as an n-ary application. For-}
{--- example, if we have "Unquote '(fn x y)", the type is the same as just-}
{--- "(fn x y)". The n-ary case is easy to reduce to pairs.-}
{-argType <- inferType' ctx arg-}
{-case typ of-}
{-Sigma typ' binding-}
checkType' ctx (Var (Left actualType)) expectedType = do
  when ((Right <$> actualType) /= expectedType)
    $ throwError
    $ TypeMismatch
      { _expected = T.pack $ show expectedType,
        _actual = T.pack $ show actualType
      }
checkType' ctx x expectedType = do
  actualType <- inferType' ctx x
  when ((Right <$> actualType) /= expectedType)
    $ throwError
    $ TypeMismatch
      { _expected = T.pack $ show expectedType,
        _actual = T.pack $ show actualType
      }

toTyped :: (HasCallStack, Show v) => Term (Either (Term v) v) -> Term (Either (Term v) v)
toTyped v = Var . Left $ fromRight <$> v

fromRight :: (HasCallStack, Show a) => Either a b -> b
fromRight (Right x) = x
fromRight (Left e) = error $ "Not right:  " ++ show e
{-
------------------------------------------------------------------------------
-- Bibliography

[MiniTT] A simple type-theoretic language: Mini-TT (Coquand et al)
[PracLev] The Practical Guide to Levitation (Al-Sibahi)
[GentleLev] The Gentle Art of Levitation (Chapman et al)
[GenElim] Generic Constructors and Eliminators from Descriptions (Diehl, Sheard)
-}
