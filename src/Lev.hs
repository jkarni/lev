module Lev where

import Lev.Internal
import Text.Trifecta
import System.IO (stdout)

import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

parseExpressionFromFile :: FilePath -> IO (Term T.Text)
parseExpressionFromFile fp = do
  result <- parseFromFileEx exprP fp
  case result of
    Failure x -> do
      Doc.displayIO stdout $ Doc.renderPretty 0.8 80 $ _errDoc x
      error "Failed"
    Success a  -> return a

parseProgramFromFile :: FilePath -> IO (Program T.Text)
parseProgramFromFile fp = do
  result <- parseFromFileEx programP fp
  case result of
    Failure x -> do
      Doc.displayIO stdout $ Doc.renderPretty 0.8 80 $ _errDoc x
      error "Failed"
    Success a  -> return a

{-typeCheckFile :: FilePath -> IO (Maybe (Term T.Text, Term T.Text))-}
{-typeCheckFile fp = do-}
  {-x <- parseFile fp-}
  {-case x of-}
    {-Nothing -> Nothing-}
    {-Just v -> case-}

{-evalExprFile :: FilePath -> IO (Term T.Text)-}
{-evalExprFile = _-}
