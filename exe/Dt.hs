module Main (main) where

import GHC.Generics        (Generic)
import Lev
import Options.Applicative

main :: IO ()
main = execParser fullOpts >>= run

run :: Options -> IO ()
run opts
  | parseOnly opts && evalExpression opts
      = parseExpressionFromFile (file opts) >>= print
  | parseOnly opts
      = parseFile (file opts) >>= print
  | typeCheckOnly opts
      = typeCheckFile (file opts)

------------------------------------------------------------------------------
-- Options
------------------------------------------------------------------------------

fullOpts :: ParserInfo Options
fullOpts = info options
               (fullDesc
              <> progDesc "The dt interpreter"
               )


options :: Parser Options
options = Options
  <$> argument str (metavar "FILE")
  <*> switch
       ( long "parse-only"
      <> short 'P'
      <> help "Just parse file and report errors"
       )
  <*> switch
       ( long "typecheck-only"
      <> short 'T'
      <> help "Just parse file and report errors"
       )
  <*> switch
       ( long "as-expression"
      <> short 'E'
      <> help "Evaluate file as single expression"
       )

data Options = Options
  { file           :: FilePath
  , parseOnly      :: Bool
  , typeCheckOnly  :: Bool
  , evalExpression :: Bool
  } deriving (Eq, Show, Read, Generic)
