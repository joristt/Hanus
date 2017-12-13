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
evalDeclaration (GlobalVarDeclaration (Variable (Identifier name) t) _) = do
    name <- newName name
    let bind = declare name (typeDefault t)
    let ret = NoBindS (VarE name)
    return $ DoE [bind, ret]

getVal :: Integer -> Q Exp
getVal value = do
    name <- newName "a"
    let bind = LetS [ValD (VarP name) (NormalB (LitE (IntegerL value))) []]
    let ret  = NoBindS (VarE name)
    let qdo  = DoE [bind, ret]
    return qdo

evalProgram :: Program -> Q [Dec]
evalProgram (Program decls) = do  
        x <- entry
        return x:[]
    where globalVars = filter filterVars decls
          procedures = filter filterProcs decls
          filterVars dec  = case dec of 
                                GlobalVarDeclaration _ _ -> True
                                otherwise                -> False
          filterProcs dec = case dec of 
                                Procedure _ _ _ -> True
                                otherwise       -> False
          entry = do 
              unit <- [e|()|]
              let body = (DoE [NoBindS unit])
              return (FunD (mkName "run") [Clause [] (NormalB body) []])

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
