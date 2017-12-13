{-# LANGUAGE TemplateHaskell #-}

module Eval where

import AST
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Maybe

globvartype = ConT $ mkName "Inttt"
globvar = GlobalVarDeclaration (Variable (Identifier "glob_var1") globvartype) (LitE (IntegerL 10))
p = Program [globvar]

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

namify :: Identifier -> Name
namify (Identifier n) = mkName n

evalGlobalVarDeclaration (GlobalVarDeclaration (Variable n t) e) = do
    let name = namify n
    return $ LetS [ValD (VarP name) (NormalB e) []]

varToPat :: Variable -> Q Pat
varToPat (Variable n t) = do
    let name = namify n
    return $ SigP (VarP name) t

evalProcedure globalArgs (Procedure n vs b) = do
    let name = namify n
    inputArgs <- mapM varToPat vs
    let pattern = TupP (globalArgs ++ inputArgs)
    let body = evalProcedureBody b pattern
    return $ FunD name [Clause [pattern] body []]

tupP2tupE :: Pat -> Exp
tupP2tupE (TupP pats) = TupE $ map (\(VarP name) -> VarE name) pats

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


