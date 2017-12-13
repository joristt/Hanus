{-# LANGUAGE TemplateHaskell #-}

module Eval where

import AST
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Maybe

evalProgram :: Program -> Q [Dec]
evalProgram (Program decls) = do  
        x <- entry
        return (x:[])
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

varToPat :: Variable -> Q Pat
varToPat (Variable n t) = do
    name <- shadow n
    return $ SigP (VarP name) t

evalProcedure globalArgs (Procedure n vs b) = do
    name <- shadow n
    let body      = evalBlock vs
    inputArgs <- mapM varToPat vs
    let pattern   = TupP (globalArgs ++ inputArgs)
    return $ FunD name [Clause [pattern] body []]

-- evaluates a Block (note that type Block = [Statement])
evalBlock ss = NormalB (ListE (map evalStatement ss))

evalStatement x = TupE []
