{-# LANGUAGE OverloadedLists #-}
module Lev.Internal.Parser where

import Control.Applicative
import Prelude                     hiding (pi)
import Text.Parser.Token.Highlight
import Text.Trifecta

import qualified Data.Text as T

import Lev.Internal.Expr

exprP :: (Monad m, TokenParsing m) => m (Term T.Text)
exprP = (parensExpP <?> "list")
    <|> (typeP <?> "type")
    <|> (varP <?> "variable")

parensExpP :: (Monad m, TokenParsing m) => m (Term T.Text)
parensExpP = parens $ annotationP <|> piP <|> applicationP <|> lambdaP

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
