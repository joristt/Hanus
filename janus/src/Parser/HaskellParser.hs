{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Parser.HaskellParser where

import Data.Either.Extra

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Syntax

myTest = parse (pType [";"]) (createStr (LineColPos 0 0 0) "Maybe [a];")

pType :: [String] -> Parser (Type, String)
pType splits = pSplitUntilSuccess "" splits (eitherToMaybe . parseType)

pExp :: [String] -> Parser (Exp, String)
pExp splits = pSplitUntilSuccess "" splits (eitherToMaybe . parseExp)

pSplitUntilSuccess :: String -> [String] -> (String -> Maybe a) -> Parser (a, String)
pSplitUntilSuccess prefix splits f = do
  (str, sep) <- pUntil splits
  case f (prefix ++ str) of
    Just a  -> return (a, sep)
    Nothing -> pSplitUntilSuccess (prefix ++ str ++ sep) splits f

pUntil :: [String] -> Parser (String, String)
pUntil splits = pSplit <<|> (\x (y, z) -> (x : y, z)) <$> pRange (minBound, maxBound) <*> pUntil splits
  where
    pSplit = (\x -> ([], x)) <$> pAny pToken splits
