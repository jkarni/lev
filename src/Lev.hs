module Lev where

import qualified Data.Text as T
import GHC.Stack (HasCallStack)
import Lev.Internal
import Prettyprinter (Pretty, pretty)
import Prettyprinter.Render.Terminal (putDoc)
import System.Exit (exitFailure)
import Text.Trifecta

-- | Parse a file as a single expression.
parseExpressionFromFile :: (HasCallStack) => FilePath -> IO (Term T.Text)
parseExpressionFromFile fp = do
  result <- parseFromFileEx exprP fp
  case result of
    Failure x -> do
      putDoc $ _errDoc x
      exitFailure
    Success a -> return a

-- | Parse a file as a full program.
parseFile :: (HasCallStack) => FilePath -> IO (Program T.Text)
parseFile fp = do
  result <- parseFromFileEx programP fp
  case result of
    Failure x -> do
      putDoc $ _errDoc x
      exitFailure
    Success a -> return a

-- | Type check a file in the empty environment (no imports).
typeCheckFile :: (HasCallStack) => FilePath -> IO (Program T.Text)
typeCheckFile fp = do
  parsedProg <- parseFile fp
  case runEnvironment $ typeCheckProgram parsedProg of
    Left e -> failWith e
    Right () -> putStrLn "Program typechecks!" >> return parsedProg

evalFile :: (HasCallStack) => FilePath -> IO ()
evalFile fp = do
  parsedProg <- parseFile fp
  case runEnvironment $ evaluateProgram parsedProg of
    Left e -> failWith e
    Right v -> putDoc $ pretty v

failWith :: Pretty a => a -> IO x
failWith msg = putDoc (pretty msg) >> exitFailure
