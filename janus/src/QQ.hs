{-# LANGUAGE TemplateHaskell #-}

module QQ where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Parser.JanusParser (parser)
import SemanticChecker (semanticCheck)
import Eval
import AST

-- | Hanus quasi-quoter.
hanus :: QuasiQuoter
hanus = QuasiQuoter {
      quoteExp = undefined
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec  = \prog-> do
        p <- runQQParser prog
        evalProgram p
    }

-- | Hanus quasi-quoter for files.
hanusF :: QuasiQuoter
hanusF = quoteFile hanus

-- | Hanus quasi-quoter for testing purposes
hanusT :: QuasiQuoter
hanusT = QuasiQuoter {
      quoteExp = undefined
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec  = \prog-> do
        p <- runQQParserT prog
        r <- evalProgramT p
        return r
    }

hanusTF :: QuasiQuoter
hanusTF = quoteFile hanusT

-- | Parse program by passing location info and doing semantic checking.
runQQParser :: String -> Q Program
runQQParser prog = do
  loc <- location
  let (row, col) = loc_start loc
  let p = parser (loc_filename loc) row col prog
  case semanticCheck p of
    Just err -> fail $
      "Semantic check failed at (line " ++ show row ++ ", col " ++ show col
        ++ "):\n\t" ++ err
    Nothing -> return p

-- | Parser for testing purposes, semantic checks are disabled
runQQParserT :: String -> Q Program
runQQParserT prog = do
  loc <- location
  let (row, col) = loc_start loc
  let p = parser (loc_filename loc) row col prog
  return p
