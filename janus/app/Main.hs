module Main where

import Options.Applicative
import Data.Semigroup ((<>))

import Lib

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
run (Options inputFile dummyInt) = do
  putStrLn someFunc
