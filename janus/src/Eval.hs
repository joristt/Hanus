{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-} 

module Eval where

import AST
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Maybe
import Control.Monad

import qualified Debug.Trace as Debug

globvartype = ConT $ mkName "Int"
stringtype = ConT $ mkName "String"
globvar = GlobalVarDeclaration (Variable (Identifier "glob_var1") globvartype) (LitE (IntegerL 10))
globvar2 = GlobalVarDeclaration (Variable (Identifier "glob_var2") globvartype) (LitE (IntegerL 10))
globvar3 = GlobalVarDeclaration (Variable (Identifier "glob_var3") stringtype) (LitE (StringL "Dag joris"))
p = Program [globvar, globvar2, globvar3]

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
              stTup <- statePattern globalVars
              let body = DoE (binds:[BindS (TupP stTup) fcall, 
                            NoBindS (AppE (VarE $ mkName "return") ((tupP2tupE . TupP) stTup))])
              return (FunD (mkName "run") [Clause [] (NormalB body) []])
          vdecs = do
              decs <- mapM genDec globalVars
              return (LetS decs)

genDec :: Declaration -> Q Dec
genDec (GlobalVarDeclaration (Variable ident t) exp) = do 
    let name = namify ident
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

evalGlobalVarDeclaration :: Declaration -> Q Stmt
evalGlobalVarDeclaration (GlobalVarDeclaration (Variable n t) e) = do
    let name = namify n
    return $ LetS [ValD (VarP name) (NormalB e) []]

evalProcedure globalArgs (Procedure n vs b) = do
    let name = namify n
    inputArgs <- mapM varToPat vs
    let pattern = TupP (globalArgs ++ inputArgs)
    let body = evalProcedureBody b pattern
    return $ FunD name [Clause [pattern] body []]

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
evalProcedureBody ss pattern = do
    NormalB $ DoE $ (concatMap evalStatement ss) ++ [returnTup]
        where returnTup = NoBindS $ tupP2tupE pattern

evalStatement (Assignement lhss expr) = evalAssignments lhss expr
evalStatement _ = error "Only Assignment can be evaluated at the moment."

evalAssignments :: [LHS] -> Exp -> [Stmt]
evalAssignments lhss expr = map (\lhs -> evalAssignment lhs expr) lhss

evalAssignment :: LHS -> Exp -> Stmt
evalAssignment (LHSIdentifier n) expr
    = LetS [ValD (VarP $ namify n) (NormalB expr) []]
evalAssignment _ _ = error "Only LHSIdentifier can be evaluated at the moment."

-- *** HELPERS *** ---

namify :: Identifier -> Name
namify (Identifier n) = mkName n

varToPat :: Variable -> Q Pat
varToPat (Variable n t) = do
    let name = namify n
    return $ SigP (VarP name) t

tupP2tupE :: Pat -> Exp
tupP2tupE (TupP pats) = Debug.trace (show pats) $ TupE $ map (\(SigP (VarP name) _) -> VarE name) pats
