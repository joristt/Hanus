{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Parser.HaskellParser where

import Data.Maybe
import Data.Either.Extra

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Language.Haskell.Meta.Parse
import Language.Haskell.TH.Syntax

pType :: [String] -> Parser (Type, String)
pType splits =
  pSplitUntilSuccess
    't'
    "type"
    (\x -> "type, \"" ++ x ++ "\" is not a type.")
    splits
    (eitherToMaybe . parseType)

pExp :: [String] -> Parser (Exp, String)
pExp splits =
  pSplitUntilSuccess
    'x'
    "Haskell expression"
    (\x -> "Haskell expression, \"" ++ x ++ "\" is not a valid expression.")
    splits (eitherToMaybe . parseExp)

pSplitUntilSuccess :: Char -> String -> (String -> String) -> [String] -> (String -> Maybe a) -> Parser (a, String)
pSplitUntilSuccess c errorMsg errorMsg2 splits f =
    ((\sep -> (fromJust $ f [c], sep)) <$ pSatisfy (const False) (Insertion errorMsg c 1) <*> pSplit)
    <<|> pSplitUntilSuccess' c errorMsg2 "" splits f
  where
    pSplit = pAny pToken splits

pSplitUntilSuccess' :: Char -> (String -> String) -> String -> [String] -> (String -> Maybe a) -> Parser (a, String)
pSplitUntilSuccess' recovery errorMsg prefix splits f = addLength 10 (do
    (str, sep) <- pUntil splits
    case f (prefix ++ str) of
      Just a  -> return (a, sep)
      Nothing ->
        -- Error correction
        (
          return (fromJust $ f [recovery], sep) <*
          pSatisfy (const False) (Insertion (errorMsg (prefix ++ str)) recovery 1)
        )
        <<|> addLength 100 (pSplitUntilSuccess' recovery errorMsg (prefix ++ str ++ sep) splits f `micro` 10)
  )

pUntil :: [String] -> Parser (String, String)
pUntil splits = pSplit <<|> (\x (y, z) -> (x : y, z)) <$> pRangeInsert (minBound, maxBound) (Insertion "any symbol (in a Haskell expression or type)" 'a' 10) <*> pUntil splits
  where
    pSplit = (\x -> ([], x)) <$> pAny pToken splits
