{-# LANGUAGE DoAndIfThenElse #-}
module Main where

import Criterion.Main
import Data.Semigroup ((<>))
import Options.Applicative
import System.Environment (withArgs)
import Parser.JanusParser (parser)

-- | Command-line options.
data Options = Options
  { inputFile :: String
  , benchmark :: Bool
  , output    :: String
  }

-- | Parsing of command-line options.
parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      (  long "input"
      <> short 'i'
      <> metavar "STRING"
      <> help "Janus source file"
      )
  <*> option auto
      (  short 'b'
      <> long "benchmark"
      <> showDefault
      <> value False
      <> metavar "BOOl"
      <> help "Run benchmark"
      )
  <*> option auto
      (  short 'o'
      <> long "output"
      <> showDefault
      <> value "report.html"
      <> metavar "STR"
      <> help "Output file for benchmark"
      )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | Main.
main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Janus DSL")

-- | Run.
run :: Options -> IO ()
run (Options inputFile benchmark output) =
  if benchmark then
    withArgs ["-o", output] $
      defaultMain [bgroup inputFile [bench "1" $ nfIO go]]
  else go
  where go = do prog <- readFile inputFile
                print $ parser "" 0 0 prog
