{-# LANGUAGE OverloadedLists #-}
module Lev.Internal.Parser where

import Control.Applicative
import Prelude                     hiding (pi)
import Text.Parser.Token.Highlight
import Text.Trifecta

import qualified Data.Text as T

import Lev.Internal.Expr

exprP :: (Monad m, TokenParsing m) => m (Term T.Text)
exprP = annotationP <|> typeP <|> applicationP <|> varP <|> piP

annotationP :: (Monad m, TokenParsing m) => m (Term T.Text)
annotationP = parens $ (-:) <$> exprP <*> exprP

typeP :: TokenParsing m => m (Term T.Text)
typeP = textSymbol "Type" *> pure Type

applicationP :: (Monad m, TokenParsing m) => m (Term T.Text)
applicationP = parens $ ($$) <$> exprP <*> exprP

varP :: (Monad m, TokenParsing m) => m (Term T.Text)
varP = variable <$> ident identStyle

piP :: (Monad m, TokenParsing m) => m (Term T.Text)
piP = parens $ do
  _ <- textSymbol "Pi"
  typ <- exprP
  var <- ident identStyle
  body <- exprP
  return $ pi var typ body

------------------------------------------------------------------------------
-- Identifiers
------------------------------------------------------------------------------

identStyle :: TokenParsing m => IdentifierStyle m
identStyle = IdentifierStyle
  { _styleName = "dt identifier"
  , _styleStart = charLiteral
  , _styleLetter = charLiteral
  , _styleReserved = []
  , _styleHighlight = Identifier
  , _styleReservedHighlight = ReservedIdentifier
  }
