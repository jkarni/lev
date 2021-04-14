{-# LANGUAGE OverloadedLists #-}

module Lev.Internal.Parser where

import Control.Applicative
import qualified Data.Text as T
import Lev.Internal.Decl
import Lev.Internal.Expr
import Text.Parser.Token.Highlight
import Text.Trifecta
import Prelude hiding (pi)

------------------------------------------------------------------------------
-- Declarations
------------------------------------------------------------------------------

programP :: (Monad m, TokenParsing m) => m (Program T.Text)
programP = Program <$> some declP

declP :: (Monad m, TokenParsing m) => m (Definition T.Text, Signature T.Text)
declP = (flip (,)) <$> signatureP <*> definitionP

definitionP :: (Monad m, TokenParsing m) => m (Definition T.Text)
definitionP = parens $ do
  _ <- symbolic '='
  var <- ident identStyle
  body <- exprP
  return $ Definition var body

signatureP :: (Monad m, TokenParsing m) => m (Signature T.Text)
signatureP = parens $ do
  _ <- colon
  var <- ident identStyle
  body <- exprP
  return $ Signature var body

------------------------------------------------------------------------------
-- Terms
------------------------------------------------------------------------------
exprP :: (Monad m, TokenParsing m) => m (Term T.Text)
exprP =
  (parensExpP <?> "list")
    <|> (tupleP <?> "tuple")
    <|> (typeP <?> "type")
    <|> (unitTypeP <?> "unit type")
    <|> (varP <?> "variable")

parensExpP :: (Monad m, TokenParsing m) => m (Term T.Text)
parensExpP =
  parens $
    (annotationP <?> "annotation")
      <|> (piP <?> "pi")
      <|> (lambdaP <?> "lambda")
      <|> (unquoteP <?> "unquote")
      <|> (applicationP <?> "application")
      <|> (unitValueP <?> "unit value")

unitTypeP :: (Monad m, TokenParsing m) => m (Term T.Text)
unitTypeP = textSymbol "Unit" *> pure UnitType

unitValueP :: (Monad m) => m (Term T.Text)
unitValueP = return UnitValue

annotationP :: (Monad m, TokenParsing m) => m (Term T.Text)
annotationP = do
  _ <- colon
  val <- exprP
  typ <- exprP
  return $ val -: typ

typeP :: TokenParsing m => m (Term T.Text)
typeP = textSymbol "Type" *> pure Type

applicationP :: (Monad m, TokenParsing m) => m (Term T.Text)
applicationP = do
  fn <- exprP
  args <- some exprP
  return $ foldl Application fn args

varP :: (Monad m, TokenParsing m) => m (Term T.Text)
varP = variable <$> ident identStyle

piP :: (Monad m, TokenParsing m) => m (Term T.Text)
piP = do
  _ <- textSymbol "pi"
  typ <- exprP
  var <- ident identStyle
  body <- exprP
  return $ pi var typ body

lambdaP :: (Monad m, TokenParsing m) => m (Term T.Text)
lambdaP = do
  _ <- textSymbol "lam"
  var <- ident identStyle
  body <- exprP
  return $ lambda var body

sigmaP :: (Monad m, TokenParsing m) => m (Term T.Text)
sigmaP = do
  _ <- textSymbol "sigma"
  typ <- exprP
  var <- ident identStyle
  body <- exprP
  return $ sigma var typ body

-- Ultimately parsed into nested pairs (i.e., a heterogenous list)
tupleP :: (Monad m, TokenParsing m) => m (Term T.Text)
tupleP = do
  _ <- char '\''
  parens $ do
    members <- some exprP
    return $ foldr Pair UnitValue members

unquoteP :: (Monad m, TokenParsing m) => m (Term T.Text)
unquoteP = do
  _ <- textSymbol "unquote"
  t <- tupleP
  return $ Unquote t

------------------------------------------------------------------------------
-- Identifiers
------------------------------------------------------------------------------

identStyle :: TokenParsing m => IdentifierStyle m
identStyle =
  IdentifierStyle
    { _styleName = "identifier",
      _styleStart = noneOf "()",
      _styleLetter = alphaNum, -- noneOf "()"
      _styleReserved = ["lam", "pi", "sigma", "unquote", ":"],
      _styleHighlight = Identifier,
      _styleReservedHighlight = ReservedIdentifier
    }
