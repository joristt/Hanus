{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module TestEval where

import AST
import Eval
import Language.Haskell.TH.Syntax

--typetest = $(typeTest Int)
--test  = $(getVal 10)
--test2 = $(evalDeclaration (GlobalVarDeclaration (Variable (Identifier "a") Int)))

$(evalProgram (progFromExp (LitE (IntegerL 45))))