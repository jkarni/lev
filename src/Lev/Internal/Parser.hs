{-# LANGUAGE OverloadedLists #-}

module Lev.Internal.Parser where

import Control.Applicative
import qualified Data.Text as T
import GHC.Generics (Generic)
import Lev.Internal.Decl
import Lev.Internal.Expr
import Text.Parser.Token.Highlight (Highlight (..))
import Text.Trifecta
  ( (<?>),
    DeltaParsing,
    IdentifierStyle (..),
    Span,
    Spanned (..),
    TokenParsing,
    char,
    colon,
    ident,
    noneOf,
    parens,
    spanned,
    symbolic,
    textSymbol,
  )
import Prelude hiding (pi, span)

------------------------------------------------------------------------------
-- Declarations
------------------------------------------------------------------------------

programP :: (DeltaParsing m) => m (Program Loc T.Text)
programP = Program <$> some declP

declP :: (DeltaParsing m) => m (Definition Loc T.Text, Signature Loc T.Text)
declP = (flip (,)) <$> signatureP <*> definitionP

definitionP :: (DeltaParsing m) => m (Definition Loc T.Text)
definitionP = parens $ do
  _ <- symbolic '='
  var <- ident identStyle
  body <- exprP
  return $ Definition var body

signatureP :: (DeltaParsing m) => m (Signature Loc T.Text)
signatureP = parens $ do
  _ <- colon
  var <- ident identStyle
  body <- exprP
  return $ Signature var body

------------------------------------------------------------------------------
-- Term Deltas
------------------------------------------------------------------------------
exprP :: (DeltaParsing m) => m (Term Loc T.Text)
exprP =
  (parensExpP <?> "list")
    <|> (tupleP <?> "tuple")
    <|> (typeP <?> "type")
    <|> (unitTypeP <?> "unit type")
    <|> (varP <?> "variable")

parensExpP :: (DeltaParsing m) => m (Term Loc T.Text)
parensExpP =
  parens $
    (annotationP <?> "annotation")
      <|> (piP <?> "pi")
      <|> (lambdaP <?> "lambda")
      <|> (recP <?> "description-rec")
      <|> (endP <?> "description-end")
      <|> (argP <?> "description-arg")
      <|> (descriptionP <?> "description")
      <|> (unquoteP <?> "unquote")
      <|> (applicationP <?> "application")
      <|> (unitValueP <?> "unit value")

unitTypeP :: (DeltaParsing m) => m (Term Loc T.Text)
unitTypeP = do
  _ :~ span <- spanned $ textSymbol "Unit"
  pure $ UnitType (Loc span)

unitValueP :: (DeltaParsing m) => m (Term Loc T.Text)
unitValueP = do
  _ :~ span <- spanned $ pure ()
  return $ UnitValue (Loc span)

annotationP :: (DeltaParsing m) => m (Term Loc T.Text)
annotationP = do
  _ <- colon
  val <- exprP
  typ <- exprP
  return $ val -: typ

typeP :: (DeltaParsing m) => m (Term Loc T.Text)
typeP = do
  _ :~ span <- spanned $ textSymbol "Type"
  pure $ Type (Loc span)

applicationP :: (DeltaParsing m) => m (Term Loc T.Text)
applicationP = do
  (fn, args) :~ span <- spanned $ (,) <$> exprP <*> some exprP
  return $ foldl (Application (Loc span)) fn args

varP :: (DeltaParsing m) => m (Term Loc T.Text)
varP = variable <$> ident identStyle

piP :: (DeltaParsing m) => m (Term Loc T.Text)
piP = do
  _ <- textSymbol "pi"
  typ <- exprP
  var <- ident identStyle
  body <- exprP
  return $ pi var typ body

lambdaP :: (DeltaParsing m) => m (Term Loc T.Text)
lambdaP = do
  (var, body) :~ span <- spanned $ do
    _ <- textSymbol "lam"
    (,) <$> ident identStyle <*> exprP
  return $ lambda (Loc span) var body

sigmaP :: (DeltaParsing m) => m (Term Loc T.Text)
sigmaP = do
  (typ, var, body) :~ span <- spanned $ do
    _ <- textSymbol "sigma"
    (,,) <$> exprP <*> ident identStyle <*> exprP
  return $ sigma (Loc span) var typ body

-- Ultimately parsed into nested pairs (i.e., a heterogenous list)
tupleP :: (DeltaParsing m) => m (Term Loc T.Text)
tupleP = do
  _ <- char '\''
  parens $ do
    members :~ span <- spanned $ some exprP
    return $ foldr (Pair (Loc span)) (UnitValue (Loc span)) members

unquoteP :: (DeltaParsing m) => m (Term Loc T.Text)
unquoteP = do
  t :~ span <- spanned $ do
    _ <- textSymbol "unquote"
    tupleP
  return $ Unquote (Loc span) t

argP :: (DeltaParsing m) => m (Term Loc T.Text)
argP = do
  (argType, argFn) :~ span <- spanned $ do
    _ <- textSymbol "description-arg"
    (,) <$> exprP <*> exprP
  return $ ArgDesc (Loc span) argType argFn

endP :: (DeltaParsing m) => m (Term Loc T.Text)
endP = do
  endType :~ span <- spanned $ do
    _ <- textSymbol "description-end"
    exprP
  return $ EndDesc (Loc span) endType

recP :: (DeltaParsing m) => m (Term Loc T.Text)
recP = do
  (recType, recDesc) :~ span <- spanned $ do
    _ <- textSymbol "description-rec"
    (,) <$> exprP <*> exprP
  return $ RecDesc (Loc span) recType recDesc

descriptionP :: (DeltaParsing m) => m (Term Loc T.Text)
descriptionP = do
  param :~ span <- spanned $ do
    _ <- textSymbol "description"
    exprP
  return $ Description (Loc span) param

------------------------------------------------------------------------------
-- Identifiers
------------------------------------------------------------------------------

identStyle :: TokenParsing m => IdentifierStyle m
identStyle =
  IdentifierStyle
    { _styleName = "identifier",
      _styleStart = noneOf "\n\t ()",
      _styleLetter = noneOf "\n\t ()", -- noneOf "()"
      _styleReserved = ["lam", "pi", "data", "sigma", "unquote", ":"],
      _styleHighlight = Identifier,
      _styleReservedHighlight = ReservedIdentifier
    }

------------------------------------------------------------------------------
-- Loc
------------------------------------------------------------------------------

data Loc
  = NoLocation
  | Loc Span
  deriving stock (Eq, Show, Generic)

instance Semigroup Loc where
  NoLocation <> b = b
  a <> NoLocation = a
  Loc a <> Loc b = Loc $ a <> b

instance Monoid Loc where
  mempty = NoLocation
