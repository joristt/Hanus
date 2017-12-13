{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-} 

module Eval where

import AST
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Maybe
import Control.Monad

import qualified Debug.Trace as Debug

-- Simple test program
globvartype = ConT $ mkName "Int"
globvar = GlobalVarDeclaration (Variable (Identifier "glob_var1") globvartype) (LitE (IntegerL 10))
progFromExp e = Program [globvar, Procedure (Identifier "main") [] [(Assignement [LHSIdentifier (Identifier "glob_var1")] e)]]

-- Evaluate a given program by generating a 'run' function that calls the 'main' procedure of the program. 
evalProgram :: Program -> Q [Dec]
evalProgram (Program decls) = do  
        -- generate program entry point ('run' function)
        x  <- entry
        -- generate pattern for program state
        pt <- statePattern globalVars
        -- generate function declarations for all procedures in program
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
          -- generates the program entry point
          entry = do
              fcall <- getMain globalVars
              binds <- vdecs
              stTup <- statePattern globalVars
              let body = DoE (binds:[NoBindS fcall])
              return (FunD (mkName "run") [Clause [] (NormalB body) []])
          -- Let bindigs for variable declarations
          vdecs = do
              decs <- mapM genDec globalVars
              return (LetS decs)

-- generate a let statement from pattern and expression of the form
-- let *pat* = *exp* to be used in do expressions
functionCall :: Pat -> Exp -> Stmt
functionCall pattern exp = LetS [ValD pattern (NormalB exp) []]

-- Generate variable declarations for global variables
genDec :: Declaration -> Q Dec
genDec (GlobalVarDeclaration (Variable ident t) exp) = do 
    let name = namify ident
    return (ValD (SigP (VarP name) t) (NormalB exp) []) 

-- Generate an expression that calls the main function with all global 
-- variables as state
getMain :: [Declaration] -> Q Exp
getMain decs = do 
    name <- [e|main|]
    args <- mapM (\(GlobalVarDeclaration (Variable (Identifier x) _) _) -> return ((VarE . mkName) x)) decs
    x <- foldM (\exp el -> return (AppE exp el)) name args
    return x

-- Generate a pattern that represents the program state 
statePattern :: [Declaration] -> Q [Pat]
statePattern varDecs = mapM toPat varDecs
    where toPat (GlobalVarDeclaration var _) = varToPat var

evalGlobalVarDeclaration :: Declaration -> Q Stmt
evalGlobalVarDeclaration (GlobalVarDeclaration (Variable n t) e) = do
    let name = namify n
    return $ LetS [ValD (VarP name) (NormalB e) []]

evalProcedure :: [Pat] -> Declaration -> Q Dec
evalProcedure globalArgs (Procedure n vs b) = do
    let name = namify n
    inputArgs <- mapM varToPat vs
    let pattern = TupP (globalArgs ++ inputArgs)
    body <- evalProcedureBody b pattern
    return $ FunD name [Clause [pattern] body []]

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
evalProcedureBody :: [Statement] -> Pat -> Q Body
evalProcedureBody ss pattern = do
    x <- concatMapM evalStatement ss
    return $ NormalB $ DoE $ x ++ [returnTup]
        where returnTup = NoBindS $ tupP2tupE pattern

evalStatement :: Statement -> Q [Stmt]
evalStatement (Assignement lhss expr) = evalAssignments lhss expr
--evalStatement (LocalVarDeclaration v e1 b e2) = evalLocalVarDeclaration v e1 b e2
evalStatement _ = error "Only Assignment can be evaluated at the moment."

evalAssignments :: [LHS] -> Exp -> Q [Stmt]
evalAssignments lhss expr = concatMapM (\lhs -> evalAssignment lhs expr) lhss

evalAssignment :: LHS -> Exp -> Q [Stmt]
evalAssignment (LHSIdentifier n) expr = do
    -- you can't do: "let x = x + 5", instead you have
    -- to do "let y = x + 5; let x = y"
    let op        = VarE (mkName "+")
    tempname     <- newName "a"
    let finalname = namify n
    let lhs2      = VarP $ namify n
    let rhs1      = NormalB $ UInfixE (VarE finalname) op expr
    let rhs2      = NormalB $ (VarE tempname)
    let stmt1     = LetS [ValD (VarP tempname) rhs1 []]
    let stmt2     = LetS [ValD (VarP finalname) rhs2 []]
    return [stmt1, stmt2]
evalAssignment _ _ = error "Only LHSIdentifier can be evaluated at the moment."

--evalLocalVarDeclaration (LocalVarDeclaration v e1 b e2)
    -- = newName "local_var_block"



-- *** HELPERS *** ---

namify :: Identifier -> Name
namify (Identifier n) = mkName n

varToPat :: Variable -> Q Pat
varToPat (Variable n t) = do
    let name = namify n
    return $ SigP (VarP name) t

tupP2tupE :: Pat -> Exp
tupP2tupE (TupP pats) = TupE $ map (\(SigP (VarP name) _) -> VarE name) pats

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (return [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; return $ x++xs
