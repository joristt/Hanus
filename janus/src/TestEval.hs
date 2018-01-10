{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-}

module TestEval where

import AST
import Eval
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import StdLib.Operator

--typetest = $(typeTest Int)
--test  = $(getVal 10)
--test2 = $(evalDeclaration (GlobalVarDeclaration (Variable (Identifier "a") Int)))

-- $(evalProgram p)

invoke :: IO ()
invoke = do
    decs <- runQ (evalProgram p)
    let result = concatMap pprint decs
    putStrLn result
