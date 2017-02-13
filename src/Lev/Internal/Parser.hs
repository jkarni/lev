{-# LANGUAGE OverloadedLists #-}
module Lev.Internal.Parser where

import Control.Applicative
import Prelude                     hiding (pi)
import Text.Parser.Token.Highlight
import Text.Trifecta

import qualified Data.Text as T

import Lev.Internal.Decl
import Lev.Internal.Expr


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
exprP = (parensExpP <?> "list")
    <|> (typeP <?> "type")
    <|> (unitTypeP <?> "unit type")
    <|> (varP <?> "variable")

parensExpP :: (Monad m, TokenParsing m) => m (Term T.Text)
parensExpP = parens
    $ (annotationP <?> "annotation")
  <|> (piP <?> "pi")
  <|> (lambdaP <?> "lambda")
  <|> (applicationP <?> "application")
  <|> (unitValueP <?> "unit value")

unitTypeP ::(Monad m, TokenParsing m) => m (Term T.Text)
unitTypeP = textSymbol "Unit" *> pure UnitType

unitValueP :: (Monad m, TokenParsing m) => m (Term T.Text)
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
applicationP = ($$) <$> exprP <*> exprP

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


------------------------------------------------------------------------------
-- Identifiers
------------------------------------------------------------------------------

identStyle :: TokenParsing m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "identifier"
  , _styleStart = alphaNum
  , _styleLetter = alphaNum
  , _styleReserved = []
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }
