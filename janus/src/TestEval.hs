{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module TestEval where

import AST
import Eval
import Language.Haskell.TH.Syntax

-- Example program that sets the first argument in the program to "sum [1..10]" and then adds 10 to it.
$(evalProgram (progFromExp (AppE (VarE $ mkName "sum") (ArithSeqE (FromToR (LitE (IntegerL 1)) (LitE (IntegerL 10)))))))