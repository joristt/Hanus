{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-} 

module Eval where

import AST
import Language.Haskell.TH.Syntax
import Language.Haskell.TH
import Data.Maybe
import Control.Monad
import qualified Data.Map as Map

import qualified Debug.Trace as Debug

type StatePatterns = Map.Map Identifier [Pat]

-- Simple test program
globvartype = ConT $ mkName "Int"
globvar = GlobalVarDeclaration (Variable (Identifier "glob_var1") globvartype) (LitE (IntegerL 10))
proc1 = Procedure (Identifier "main") [] [Call (Identifier "add10") []]
proc2 = Procedure (Identifier "add10") [] [(Assignement [LHSIdentifier (Identifier "glob_var1")] (LitE (IntegerL 10)))]
p = Program [globvar, proc1, proc2]

-- Evaluate a given program by generating a 'run' function that calls the 'main' procedure of the program. 
evalProgram :: Program -> Q [Dec]
evalProgram p@(Program decls) = do  
        -- fetch function state information
        stPats <- getStatePatterns p
        -- generate program entry point ('run' function)
        x  <- entry
        -- generate pattern for program state
        pt <- statePattern globalVars
        -- generate function declarations for all procedures in program
        fdecs <- mapM (evalProcedure stPats pt) procedures
        return $ (x:fdecs)
    where procedures = getProcedures p
          globalVars = getVariableDecs p
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

evalProcedure :: StatePatterns -> [Pat] -> Declaration -> Q Dec
evalProcedure stPatterns globalArgs (Procedure n vs b) = do
    let name = namify n
    inputArgs <- mapM varToPat vs
    let pattern = TupP (globalArgs ++ inputArgs)
    body <- evalProcedureBody stPatterns b pattern
    return $ FunD name [Clause [pattern] body []]

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
evalProcedureBody :: StatePatterns -> [Statement] -> Pat -> Q Body
evalProcedureBody stPatterns ss pattern = do
    x <- concatMapM (evalStatement stPatterns) ss
    return $ NormalB $ DoE $ x ++ [returnTup]
        where returnTup = NoBindS $ tupP2tupE pattern

evalStatement :: StatePatterns -> Statement -> Q [Stmt]
evalStatement _ (Assignement lhss expr)             = evalAssignments lhss expr
evalStatement stPatterns (Call (Identifier i) args) = evalFunctionCall stPatterns i args
evalStatement _ _ = error "Only Assignment can be evaluated at the moment."

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

evalFunctionCall :: StatePatterns -> String -> [LHS] -> Q [Stmt]
evalFunctionCall stPatterns name args = do
    let rPat = TupP pattern
    f <- foldM (\exp pat -> do
                    arg <- expFromVarP pat
                    return (AppE exp arg))
         ((VarE . mkName) name) pattern
    return [(functionCall rPat f)] 
    where pattern = case Map.lookup (Identifier name) stPatterns of
                        (Just pat) -> pat
                        Nothing    -> error "call to unknown function" 

-- *** HELPERS *** ---

namify :: Identifier -> Name
namify (Identifier n) = mkName n

varToPat :: Variable -> Q Pat
varToPat (Variable n t) = do
    let name = namify n
    return $ SigP (VarP name) t

expFromVarP :: Pat -> Q Exp
expFromVarP (SigP (VarP name) _) = return (VarE name) 

tupP2tupE :: Pat -> Exp
tupP2tupE (TupP pats) = TupE $ map (\(SigP (VarP name) _) -> VarE name) pats

concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (return [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; return $ x++xs

getStatePatterns :: Program -> Q StatePatterns
getStatePatterns program = do 
    patterns <- mapM (\(Procedure _ args _) -> getP args) procedures
    let identifiers = map (\(Procedure identifier _ _) -> identifier) procedures
    return $ Map.fromList (zip identifiers patterns)
    where procedures = getProcedures program
          getP args  = do
                globals <- statePattern (getVariableDecs program)
                argPats <- mapM varToPat args
                return $ globals ++ argPats

getProcedures :: Program -> [Declaration]
getProcedures (Program xs) = filter filterProcs xs
    where filterProcs dec = case dec of 
                                Procedure _ _ _ -> True
                                otherwise       -> False

getVariableDecs :: Program -> [Declaration]
getVariableDecs (Program decs) = filter filterVars decs
    where filterVars dec  = case dec of 
                                GlobalVarDeclaration _ _ -> True
                                otherwise                -> False
