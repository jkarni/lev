{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Lev.Internal.Expr where

import Bound
import qualified Bound.Name as BN
import Bound.Scope (hoistScope)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Bifunctor
import Data.Function (on)
import Data.Functor.Classes
import Data.Functor.Classes.Generic
import qualified Data.Map as Map
import Data.String
import Data.String.Conversions (ConvertibleStrings, convertString)
import qualified Data.Text as T
import GHC.Generics (Generic, Generic1)
import GHC.Stack (HasCallStack)
import Prettyprinter ((<+>), Pretty (..), indent, line, squotes)

------------------------------------------------------------------------------

-- * Unbound
------------------------------------------------------------------------------

class (Ord a, Show a) => Unbound a where
  unbound :: a

instance Unbound String where unbound = "_"

instance Unbound T.Text where unbound = "_"

instance Unbound v => Unbound (Either (Term b v) v) where unbound = Right unbound

------------------------------------------------------------------------------

-- * Terms
------------------------------------------------------------------------------

-- See https://github.com/ekmett/bound/issues/74 for the motivation for this.
data One = One
  deriving stock (Eq, Show, Ord, Generic, Read)

type Name = BN.Name T.Text One

-- | Inferrable or checkable terms.
data Term b a
  = -- Inferrable
    Annotation b (Term b a) (Term b a) -- (: val typ)
  | Type b -- Type
  | Application b (Term b a) (Term b a) -- (fn val)
  | Var b a -- x
  | Pi b (Term b a) Name (Scope Name (Term b) a) -- (pi X x x)
  | Sigma b (Term b a) Name (Scope Name (Term b) a) -- (Sigma X x x)
  | UnitType b -- Unit
  | UnitValue b -- ()
  | Tag b T.Text -- #Zero
  | TagType b -- Tag
      -- Macros
  | Unquote b (Term b a)
  | -- Description
    Description b (Term b a)
  | EndDesc b (Term b a)
  | RecDesc b (Term b a) (Term b a)
  | ArgDesc b (Term b a) (Term b a)
  | -- Checkable
    Pair b (Term b a) (Term b a) -- '(x y)
  | Lambda b Name (Scope Name (Term b) a) -- (lam x x)
  deriving stock (Functor, Foldable, Traversable, Generic, Generic1)

instance (Eq b, Monoid b) => Eq1 (Term b) where
  liftEq = liftEqDefault

instance (Show b, Monoid b) => Show1 (Term b) where
  liftShowsPrec = liftShowsPrecDefault

instance (Ord b, Monoid b) => Ord1 (Term b) where
  liftCompare = liftCompareDefault

instance Monoid b => Applicative (Term b) where
  pure = Var mempty
  (<*>) = ap

instance (Monoid b) => Monad (Term b) where
  {-# INLINE (>>=) #-}
  x >>= f = case x of
    Annotation loc a b -> Annotation loc (a >>= f) (b >>= f)
    Type loc -> Type loc
    Application loc a b -> Application loc (a >>= f) (b >>= f)
    Var loc a -> f a
    Pi loc a b c -> Pi loc (a >>= f) b (c >>>= f)
    Sigma loc a b c -> Sigma loc (a >>= f) b (c >>>= f)
    UnitType loc -> UnitType loc
    UnitValue loc -> UnitValue loc
    Tag loc a -> Tag loc a
    TagType loc -> TagType loc
    Unquote loc a -> Unquote loc (a >>= f)
    Description loc a -> Description loc (a >>= f)
    EndDesc loc a -> EndDesc loc (a >>= f)
    RecDesc loc a b -> RecDesc loc (a >>= f) (b >>= f)
    ArgDesc loc a b -> ArgDesc loc (a >>= f) (b >>= f)
    Pair loc a b -> Pair loc (a >>= f) (b >>= f)
    Lambda loc a b -> Lambda loc a (b >>>= f)

instance Bifunctor Term where
  second = fmap
  first f x = case x of
    Annotation loc a b -> Annotation (f loc) (first f a) (first f b)
    Type loc -> Type (f loc)
    Application loc a b -> Application (f loc) (first f a) (first f b)
    Var loc a -> Var (f loc) a
    Pi loc a b c -> Pi (f loc) (first f a) b (hoistScope (first f) c)
    Sigma loc a b c -> Sigma (f loc) (first f a) b (hoistScope (first f) c)
    UnitType loc -> UnitType (f loc)
    UnitValue loc -> UnitValue (f loc)
    Tag loc a -> Tag (f loc) a
    TagType loc -> TagType (f loc)
    Unquote loc a -> Unquote (f loc) (first f a)
    Description loc a -> Description (f loc) (first f a)
    EndDesc loc a -> EndDesc (f loc) (first f a)
    RecDesc loc a b -> RecDesc (f loc) (first f a) (first f b)
    ArgDesc loc a b -> ArgDesc (f loc) (first f a) (first f b)
    Pair loc a b -> Pair (f loc) (first f a) (first f b)
    Lambda loc a b -> Lambda (f loc) a (hoistScope (first f) b)

fvoid :: Bifunctor f => f a b -> f () b
fvoid = first (const ())

instance (Show a) => Show (Term b a) where showsPrec i x = showsPrec1 i $ fvoid x

instance (Eq a) => Eq (Term b a) where (==) = eq1 `on` fvoid

instance (Ord a) => Ord (Term b a) where compare = compare1 `on` fvoid

instance (Monoid b, IsString a) => IsString (Term b a) where
  fromString = Var mempty . fromString

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

pi :: (Monoid b, ConvertibleStrings a T.Text, Eq a) => a -> Term b a -> Term b a -> Term b a
pi var typ term = Pi mempty typ (mkName var) $ abstractT var term

fnType :: (Unbound a, Monoid b) => Term b a -> Term b a -> Term b a
fnType arg term =
  let k a = F (return a)
   in Pi mempty arg (BN.Name "_" One) $ Scope (liftM k term)

sigma :: (ConvertibleStrings a T.Text, Monoid b, Eq a) => b -> a -> Term b a -> Term b a -> Term b a
sigma loc var typ term = Sigma loc typ (mkName var) $ abstractT var term

lambda :: (Monoid b, ConvertibleStrings a T.Text, Eq a) => b -> a -> Term b a -> Term b a
lambda loc var term = Lambda loc (mkName var) $ abstractT var term

mkName :: ConvertibleStrings a T.Text => a -> Name
mkName var = BN.Name (convertString var) One

pair :: Monoid b => Term b a -> Term b a -> Term b a
pair = Pair mempty

variable :: Monoid b => a -> Term b a
variable = pure

($$) :: Monoid b => Term b a -> Term b a -> Term b a
($$) = Application mempty

(-:) :: Monoid b => Term b a -> Term b a -> Term b a
(-:) = Annotation mempty

unquote :: Term b a -> Term b a
unquote p@(Pair loc _ _) = foldl1 (Application loc) $ unfoldPair p
  where
    unfoldPair (Pair _ a b) = a : unfoldPair b
    unfoldPair (UnitValue _) = []
    unfoldPair x = [x]
unquote x = x

------------------------------------------------------------------------------

-- * Context
------------------------------------------------------------------------------

newtype Context b v = Context {getCtx :: Map.Map v (Term b v)}
  deriving (Eq, Show, Ord, Generic)

emptyCtx :: Context b v
emptyCtx = Context Map.empty

lookupCtx :: Ord v => v -> Context b v -> Maybe (Term b v)
lookupCtx v (Context ctx) = Map.lookup v ctx

extendCtx :: Ord v => v -> Term b v -> Context b v -> Context b v
extendCtx var typ (Context ctx) = Context $ Map.insert var typ ctx

------------------------------------------------------------------------------

-- * Evaluation
------------------------------------------------------------------------------

nf :: (HasCallStack, Monoid b) => Term b a -> Term b a
nf (Annotation loc e y) = Annotation loc (nf e) (nf y)
nf (Type loc) = Type loc
nf (Application loc fn val) = case nf fn of
  Lambda _ _ x -> nf (instantiate1 val x)
  fn' -> Application loc fn' (nf val)
nf (Var loc x) = Var loc x
nf (Lambda loc v x) = Lambda loc v $ toScope $ nf $ fromScope x
nf (Pi loc typ v x) = Pi loc (nf typ) v (toScope $ nf $ fromScope x)
nf (Sigma loc typ v x) = Sigma loc (nf typ) v (toScope $ nf $ fromScope x)
nf (Pair loc a b) = Pair loc (nf a) (nf b)
nf (UnitType loc) = UnitType loc
nf (UnitValue loc) = UnitValue loc
nf (TagType loc) = TagType loc
nf (Description loc x) = Description loc $ nf x
nf (EndDesc loc x) = EndDesc loc $ nf x
nf (RecDesc loc x y) = RecDesc loc (nf x) (nf y)
nf (ArgDesc loc x y) = ArgDesc loc (nf x) (nf y)
nf (Tag loc x) = Tag loc x
nf (Unquote loc x) = nf $ unquote x

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

newtype M b a = M {runM' :: ExceptT (TypeError, [b]) (Reader [b]) a}
  deriving stock (Generic)
  deriving newtype
    ( Functor,
      Applicative,
      Monad,
      MonadReader [b],
      MonadError (TypeError, [b])
    )

runM :: M b a -> Either (TypeError, [b]) a
runM a = runReader (runExceptT $ runM' a) mempty

throw :: TypeError -> M b a
throw err = do
  r <- ask
  throwError (err, r)

-- | Add a location
withLoc :: b -> M b a -> M b a
withLoc loc = local (loc :)

-- We use "Either (Term v) v" for variable type, indicating that it is either a
-- name or a type. As we move under binders, we transform bound names into
-- types.

inferType :: (HasCallStack, Monoid b, Unbound v) => Context b v -> Term b v -> M b (Term b v)
inferType ctx t = inferType' ctx (Right <$> t)

-- |
-- TODO: Indicate that inferred types have loc that refers to, rather than is,
-- a place in source code
inferType' ::
  (HasCallStack, Monoid b, Unbound v) =>
  Context b v ->
  Term b (Either (Term b v) v) ->
  M b (Term b v)
inferType' ctx term = case term of
  Annotation loc e an -> do
    checkType' ctx an (Type loc)
    checkType' ctx e an
    return $ fromRight <$> an
  Application loc fn val -> do
    fnType' <- inferType' ctx fn
    case fnType' of
      Pi _ arg _ binding -> do
        checkType' ctx (Right <$> arg) (Type loc)
        return $ fromRight <$> instantiate1 val (Right <$> binding)
      _ -> throw $ CantApply $ T.pack $ show fnType'
  Var loc (Left e) -> do
    return e -- local variable
  Var loc (Right x) -> case lookupCtx x ctx of -- global variable
    Nothing -> throw $ UnknownIdentifier $ T.pack $ show x
    Just v -> return v
  Pi loc typ _ binding -> do
    checkType' ctx typ (Type loc)
    checkType' ctx (instantiate1 (Type loc) binding) (Type loc)
    return $ Type loc
  Sigma loc typ _ binding -> do
    checkType' ctx typ (Type loc)
    checkType' ctx (instantiate1 (Type loc) binding) (Type loc)
    return $ Type loc
  Type loc -> return $ Type loc
  UnitType loc -> return $ Type loc
  UnitValue loc -> return $ UnitType loc
  Tag loc _ -> return $ TagType loc
  TagType loc -> return $ Type loc
  Description loc x -> do
    checkType' ctx x (Type loc)
    return $ Type loc
  Unquote loc (Pair _ x (UnitValue _)) -> inferType' ctx x
  Unquote loc p -> do
    -- "Unquote" takes an n-tuple and treats it as an n-ary application. For
    -- example, if we have "Unquote '(fn x y)", the type is the same as just
    -- "(fn x y)".
    inferType' ctx $ unquote p
  Lambda loc _ _ -> withLoc loc $ throw $ CantInfer "lambda"
  Pair loc _ _ -> withLoc loc $ throw $ CantInfer "pair"
  EndDesc loc i -> do
    t <- inferType' ctx i
    return $ Description loc t
  RecDesc loc _ _ -> throw $ CantInfer "description-rec"
  ArgDesc loc _ _ -> throw $ CantInfer "description-arg"

checkType ::
  (HasCallStack, Monoid b, Unbound v) =>
  Context b v ->
  Term b v ->
  Term b v ->
  M b ()
checkType ctx term typ = checkType' ctx (Right <$> term) (Right <$> typ)

checkType' ::
  (HasCallStack, Monoid b, Unbound v) =>
  Context b v ->
  Term b (Either (Term b v) v) ->
  Term b (Either (Term b v) v) ->
  M b ()
checkType' ctx (Lambda loc _ e) (Pi _ t _ b) = withLoc loc $ do
  -- (: (lambda x x) (pi Int t Int))
  -- In the lambda we instantiate with 'Left t', meaning the *type* of the
  -- variable must be 't'. In pi, we instantiate with 'Right t', meaning the
  -- variable itself must
  checkType'
    ctx
    (instantiate1 (toTyped t) e)
    (instantiate1 t b)
checkType' ctx (Pair loc a b) (Sigma _ t _ t') = do
  checkType' ctx a t
  checkType' ctx b (instantiate1 t t')
checkType' ctx (EndDesc loc x) (Description _ typ) = checkType' ctx x typ
checkType' ctx (RecDesc _ a b) (Description loc' typ) = do
  checkType' ctx a typ
  checkType' ctx b (Description loc' typ)
checkType' ctx (ArgDesc loc a b) d@(Description _ _) = do
  checkType' ctx a (Type loc)
  checkType' ctx b (fnType a d)
{-checkType' ctx (Unquote (Pair fn arg)) typ -> do-}
{--- "Unquote" takes an n-tuple and treats it as an n-ary application. For-}
{--- example, if we have "Unquote '(fn x y)", the type is the same as just-}
{--- "(fn x y)". The n-ary case is easy to reduce to pairs.-}
{-argType <- inferType' ctx arg-}
{-case typ of-}
{-Sigma typ' binding-}
checkType' ctx (Var loc (Left actualType)) expectedType = do
  when ((Right <$> actualType) /= expectedType)
    $ throw
    $ TypeMismatch
      { _expected = T.pack $ show expectedType,
        _actual = T.pack $ show actualType
      }
checkType' ctx x expectedType = do
  actualType <- inferType' ctx x
  when ((Right <$> actualType) /= expectedType)
    $ throw
    $ TypeMismatch
      { _expected = T.pack $ show expectedType,
        _actual = T.pack $ show actualType
      }

toTyped ::
  (HasCallStack, Monoid b, Show v) =>
  Term b (Either (Term b v) v) ->
  Term b (Either (Term b v) v)
toTyped v = Var mempty . Left $ fromRight <$> v

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
