{-# LANGUAGE TemplateHaskell #-}

module TestEval where

import AST
import Eval
import Language.Haskell.TH.Syntax

test  = $(getVal 10)
test2 = $(evalDeclaration (GlobalVarDeclaration (Variable (Identifier "a") Int)))