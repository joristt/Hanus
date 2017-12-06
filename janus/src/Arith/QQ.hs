{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE FlexibleContexts, FlexibleInstances, GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Arith.QQ where

import Control.Monad
import Data.Generics (extQ)
import Language.Haskell.TH
import Language.Haskell.TH.Quote
import Language.Haskell.Meta (parseExp)

import Arith.AST
import Arith.Parser

import Debug.Trace

-- | Arith quasi-quoter.
arith :: QuasiQuoter
arith = QuasiQuoter {
      quoteExp = undefined
    , quotePat = undefined
    , quoteType = undefined
    , quoteDec  = \prog-> do
      p@(Prog name expr) <- runQQParser prog True
      exp <- [e| expr |] -- Normal program
      exp' <- [e| -expr |] -- Reverse program
      return [decl name exp, decl (name ++ "'") exp']
    }
    where
      decl name ex = FunD (mkName name) [Clause [] (NormalB ex) []]

-- | Arith quasi-quoter for files.
arithF :: QuasiQuoter
arithF = quoteFile arith

-- | Parse program by passing location info and doing semantic checking.
runQQParser :: String -> Bool -> Q Prog
runQQParser prog toCheck = do
  loc <- location
  let pos = (loc_filename loc, fst (loc_start loc), snd (loc_start loc))
  p@(Prog name expr) <- runParserWithLoc pos prog
  when (toCheck && not (semanticChecker expr)) $ fail "Semantic check failed!"
  return p
