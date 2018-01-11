{-# LANGUAGE TemplateHaskell #-}

module QQ where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import Parser.JanusParser (parser)
import SemanticChecker (semanticCheck)
import Eval
import AST

-- | Arith quasi-quoter.
hanus :: QuasiQuoter
hanus = QuasiQuoter {
      quoteExp = undefined
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec  = \prog-> do
        p <- runQQParser prog
        evalProgram p
    }
    where
      decl name ex = FunD (mkName name) [Clause [] (NormalB ex) []]

-- | Arith quasi-quoter for files.
hanusF :: QuasiQuoter
hanusF = quoteFile hanus

-- | Parse program by passing location info and doing semantic checking.
runQQParser :: String -> Q Program
runQQParser prog = do
  loc <- location
  let (row, col) = loc_start loc
  let p = parser (loc_filename loc) row col prog
  --unless (semanticCheck p) $ fail "Semantic check failed!"
  return p
