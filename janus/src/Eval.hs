{-# LANGUAGE TemplateHaskell #-} 

module Eval where

import AST
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Maybe

evalProgram :: Program -> Q [Dec]
evalProgram (Program decls) = do  
        x <- entry
        fdecs <- mapM (evalProcedure statePattern) procedures
        return (x:fdecs)
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

statePattern :: [GlobalVarDeclaration] -> Q [Pat]
statePattern varDecs = patterns (map toPat varDecs)
    where toPat (GlobalVarDeclaration var _) = varToPat var

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
