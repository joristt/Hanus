{-# LANGUAGE TemplateHaskell #-}

module Eval where

import AST
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Maybe

declare :: Name -> Lit -> Stmt
declare n l = LetS [ValD (VarP n) (NormalB (LitE l)) []]

shadow :: Identifier -> Q Name
shadow (Identifier n) = newName n

evalGlobalVarDeclaration (GlobalVarDeclaration (Variable n t) e) = do
    name <- shadow n
    return (name, LetS [ValD (VarP name) (NormalB e) []])

evalProcedure (Procedure n vs b) = do
    name <- shadow n
    body <- evalBlock s
    let args = map VarP vs
    FunD name [Clause args body []]

evalBlock stmts s = do


----evalDeclaration :: GlobalVarDeclaration -> 
--evalDeclaration (GlobalVarDeclaration (Variable (Identifier n) t) e)
--	= LetS [ValD (VarP n) (NormalB (LitE l)) []]

--    name <- newName name
--    let bind = declare name (typeDefault t)
--    let ret = NoBindS (VarE name)
--    return $ DoE [bind, ret]

--getVal :: Integer -> Q Exp
--getVal value = do
--    name <- newName "a"
--    let bind = LetS [ValD (VarP name) (NormalB (LitE (IntegerL value))) []]
--    let ret  = NoBindS (VarE name)
--    let qdo  = DoE [bind, ret]
--    return qdo