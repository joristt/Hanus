{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-} 

module Eval where

import AST
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Maybe
import Control.Monad

globvartype = ConT $ mkName "Int"
globvar = GlobalVarDeclaration (Variable (Identifier "glob_var1") globvartype) (LitE (IntegerL 10))
p = Program [globvar]

evalProgram :: Program -> Q [Dec]
evalProgram (Program decls) = do  
        x  <- entry
        pt <- statePattern globalVars
        fdecs <- mapM (evalProcedure pt) procedures
        return $ (x:fdecs)
    where globalVars = filter filterVars decls
          procedures = filter filterProcs decls
          filterVars dec  = case dec of 
                                GlobalVarDeclaration _ _ -> True
                                otherwise                -> False
          filterProcs dec = case dec of 
                                Procedure _ _ _ -> True
                                otherwise       -> False
          entry = do
              fcall <- getMain globalVars
              binds <- vdecs
              let body = (DoE (binds:[NoBindS fcall]))
              return (FunD (mkName "run") [Clause [] (NormalB body) []])
          vdecs = do
              decs <- mapM genDec globalVars
              return (LetS decs)

genDec :: Declaration -> Q Dec
genDec (GlobalVarDeclaration (Variable ident t) exp) = do 
    name <- shadow ident
    return (ValD (SigP (VarP name) t) (NormalB exp) []) 

getMain :: [Declaration] -> Q Exp
getMain decs = do 
    name <- [e|main|]
    args <- mapM (\(GlobalVarDeclaration (Variable (Identifier x) _) _) -> return ((VarE . mkName) x)) decs
    x <- foldM (\exp el -> return (AppE exp el)) name args
    return x

statePattern :: [Declaration] -> Q [Pat]
statePattern varDecs = mapM toPat varDecs
    where toPat (GlobalVarDeclaration var _) = varToPat var

shadow :: Identifier -> Q Name
shadow (Identifier n) = newName n

evalGlobalVarDeclaration :: Declaration -> Q (Name, Stmt)
evalGlobalVarDeclaration (GlobalVarDeclaration (Variable n t) e) = do
    name <- shadow n
    return (name, LetS [ValD (VarP name) (NormalB e) []])

varToPat :: Variable -> Q Pat
varToPat (Variable n t) = do
    name <- shadow n
    return $ SigP (VarP name) t

evalProcedure globalArgs (Procedure n vs b) = do
    name <- shadow n
    let body = evalBlock vs
    inputArgs <- mapM varToPat vs
    let pattern = TupP (globalArgs ++ inputArgs)
    return $ FunD name [Clause [pattern] body []]

-- evaluates a Block (note that type Block = [Statement])
evalBlock ss = NormalB (ListE (map evalStatement ss))

evalStatement x = TupE []
