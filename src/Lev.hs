module Lev where

import Lev.Internal
import Text.Trifecta
import System.IO (stdout)

import qualified Data.Text as T
import qualified Text.PrettyPrint.ANSI.Leijen as Doc

-- | Parse a file as a single expression.
parseExpressionFromFile :: FilePath -> IO (Term T.Text)
parseExpressionFromFile fp = do
  result <- parseFromFileEx exprP fp
  case result of
    Failure x -> do
      Doc.displayIO stdout $ Doc.renderPretty 0.8 80 $ _errDoc x
      error "Failed"
    Success a  -> return a

-- | Parse a file as a full program.
parseFile :: FilePath -> IO (Program T.Text)
parseFile fp = do
  result <- parseFromFileEx programP fp
  case result of
    Failure x -> do
      Doc.displayIO stdout $ Doc.renderPretty 0.8 80 $ _errDoc x
      error "Failed"
    Success a  -> return a

-- | Type check a file in the empty environment (no imports).
typeCheckFile :: FilePath -> IO ()
typeCheckFile fp = do
  parsedProg <- parseFile fp
  case runEnvironment $ typeCheckProgram parsedProg of
    Left e -> error e
    Right () -> return ()
