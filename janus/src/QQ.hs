{-# LANGUAGE TemplateHaskell #-}

module QQ where

import Control.Monad
import Language.Haskell.TH
import Language.Haskell.TH.Quote

import AST

type Position = ( String -- filename
                , Int    -- row
                , Int    -- column
                )

runParserWithLoc :: Position -> String -> Program
runParserWithLoc _ _ = Program [] -- TODO rebase onto #41

semanticCheck :: Program -> Bool
semanticCheck _ = True -- TODO rebase onto #43

-- | Arith quasi-quoter.
hanus :: QuasiQuoter
hanus = QuasiQuoter {
      quoteExp = undefined
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec  = \prog-> do
        p <- runQQParser prog
        exp <- [e| 10 |] -- Normal program
        exp' <- [e| -10 |] -- Reverse program
        return [decl "decl" exp, decl "decl'" exp']
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
  let pos = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))
  let p = runParserWithLoc pos prog
  unless (semanticCheck p) $ fail "Semantic check failed!"
  return p
