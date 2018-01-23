{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))
import Parser.JanusParser

import Parser.JanusParser

-- | Command-line options.
newtype Options = Options { inputFile :: String }

-- | Parsing of command-line options.
parseOptions :: Parser Options
parseOptions = Options
  <$> strOption
      (  long "input"
      <> short 'i'
      <> metavar "STRING"
      <> help "Janus source file"
      )

-- | Main.
main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Janus DSL")
  where
    withInfo :: Parser a -> String -> ParserInfo a
    withInfo opts desc = info (helper <*> opts) $ progDesc desc
    run :: Options -> IO ()
    run (Options inputFile) = do
      prog <- readFile inputFile
      print $ parser "" 0 0 prog
