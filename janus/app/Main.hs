{-# LANGUAGE DoAndIfThenElse #-}
{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, QuasiQuotes, FlexibleContexts #-}

module Main where

import Criterion.Main
import Control.Monad
import Data.Semigroup ((<>))
import Options.Applicative
import System.Environment (withArgs)
import Parser.JanusParser (parser)

import QQ
import StdLib.Operator
import StdLib.DefaultValue

-- | Command-line options.
data Options = Options
  { input     :: Int
  , benchmark :: Bool
  , output    :: String
  }

-- | Parsing of command-line options.
parseOptions :: Parser Options
parseOptions = Options
  <$> option auto
      (  short 'n'
      <> metavar "INT"
      <> help "Janus source file"
      )
  <*> switch
      (  short 'b'
      <> long "benchmark"
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
main = runMain =<< execParser (parseOptions `withInfo` "Janus DSL")

[hanusTF|examples/testrle.janus|]

-- | Run.
runMain :: Options -> IO ()
runMain (Options input benchmark output) =
  if benchmark then
    withArgs ["-o", output] $
      defaultMain [bgroup "t" [bench "1" $ whnfIO go]]
  else go
  where go = print $ hanus_main (replicate 32 1) []
    -- forM_ [1..1000] (\_ -> do let (_, z) = hanus_main (replicate 32 1) []
       --                                print $ main' [] z)
