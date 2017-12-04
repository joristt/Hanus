{-# LANGUAGE QuasiQuotes #-}

module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Arith.AST
import Arith.QQ (arith, arithF)
import Arith.Eval (eval)

-- | Command-line options.
data Options = Options
  { inputFile :: String
  , intOpt    :: Int
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
      (  long "dummy-int"
      <> showDefault
      <> value 1
      <> metavar "INT"
      <> help "Dummy integer argument"
      )

withInfo :: Parser a -> String -> ParserInfo a
withInfo opts desc = info (helper <*> opts) $ progDesc desc

-- | Main.
main :: IO ()
main = run =<< execParser (parseOptions `withInfo` "Janus DSL")

-- | Run.
run :: Options -> IO ()
run (Options inputFile dummyInt) =
  print $ eval [arith|
    0 +
    ((2 + 3) +
    (1 + (1 + 1)) + `calcNum 7`)
  |]
  where
    calcNum = (+ sum [1..5]) :: Integer -> Integer
