{-# LANGUAGE TemplateHaskell #-}

module Eval where

import AST
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Maybe

typeDefault :: AST.Type -> Lit
typeDefault Int    = IntegerL 0
typeDefault String = StringL ""

declare :: Name -> Lit -> Stmt
declare n l = LetS [ValD (VarP n) (NormalB (LitE l)) []]

--evalDeclaration :: GlobalVarDeclaration -> 
evalDeclaration (GlobalVarDeclaration (Variable (Identifier name) t)) = do
    name <- newName name
    let bind = declare name (typeDefault t)
    let ret = NoBindS (VarE name)
    return $ DoE [bind, ret]


--getVal :: Q Exp
--getVal value = do
--    name <- newName "a"
--    decAndAsg <- LetS [ValD (VarP name)]
--    --let bind = LetS decAndAsg
--    --let ret  = NoBindS (VarE name)
--    --let qdo  = DoE [bind, ret]
--    return qdo


getVal :: Integer -> Q Exp
getVal value = do
    name <- newName "a"
    let bind = LetS [ValD (VarP name) (NormalB (LitE (IntegerL value))) []]
    let ret  = NoBindS (VarE name)
    let qdo  = DoE [bind, ret]
    return qdo