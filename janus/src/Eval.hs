{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-} 

module Eval where

import AST
import StdLib.Operator

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import Control.Monad
import Control.Monad.State

import qualified Data.Map as Map
import Data.Maybe

import qualified Debug.Trace as Debug

trace x = Debug.trace (show x) x

-- Map of patterns representing the in- and output
-- data of procedures (including both global as well as
-- local state information)
type StatePatterns = Map.Map Identifier [Pat]

-- Example program for debugging purposes
globvartype = ConT $ mkName "Int"
globvar1 = GlobalVarDeclaration (Variable (Identifier "glob_var1") globvartype) (LitE (IntegerL 10))
globvar2 = GlobalVarDeclaration (Variable (Identifier "glob_var2") globvartype) (LitE (IntegerL 1))
proc1 = Procedure (Identifier "main'") [] [Assignement "+=" [LHSIdentifier (Identifier "glob_var2")] (LitE (IntegerL 10)), Call (Identifier "substract") []]
proc2 = Procedure (Identifier "substract") [] [(Assignement "-=" [LHSIdentifier (Identifier "glob_var1")] ((VarE . mkName) "glob_var2"))]
p = Program [globvar1, globvar2, proc1, proc2]

-- Generates program entry points for running the program both forward and backward, as well as
-- function declarations for all procedures in the janus program. 
--
-- The purpose of this function is to generate a TH object that represents a program equivalent 
-- to the given janus program. The resulting TH object can then be spliced and run from within 
-- another file  
evalProgram :: Program -> Q [Dec]
evalProgram p@(Program decls) = do  
    -- fetch function state information
    stPats <- getStatePatterns p
    nameFwd <- newName "run"
    nameBwd <- newName "run_bwd"
    -- generate program entry point ('run' function)
    x  <- entry nameFwd nameBwd
    -- generate pattern for program state
    pt <- statePattern globalVars
    -- generate function declarations for all procedures in program
    fdecs <- mapM (evalProcedure stPats pt) procedures
    return $ (x:fdecs)
    where procedures = getProcedures p
          globalVars = getVariableDecs p
          -- generates the program entry point
          entry fwd bwd = do
              fcall <- getMain globalVars
              binds <- vdecs
              stTup <- statePattern globalVars
              let body = DoE (binds:[NoBindS fcall])
              return (FunD fwd [Clause [] (NormalB body) []])
          -- Let bindigs for variable declarations
          vdecs = do
              decs <- mapM genDec globalVars
              return (LetS decs)

-- generate a let statement from pattern and expression of the form
-- let *pat* = *exp* to be used in do expressions
letStmt :: Pat -> Exp -> Stmt
letStmt pattern exp = LetS [ValD pattern (NormalB exp) []]

-- Generate variable declarations for global variables
genDec :: Declaration -> Q Dec
genDec (GlobalVarDeclaration (Variable ident t) exp) = do 
    let name = namify ident
    return (ValD (SigP (VarP name) t) (NormalB exp) []) 

-- Generate an expression that calls the main function with all global 
-- variables as state
getMain :: [Declaration] -> Q Exp
getMain decs = do 
    name <- [e|main'|]
    args <- mapM (\(GlobalVarDeclaration (Variable (Identifier x) _) _) -> 
        return ((VarE . mkName) x)) decs
    x <- foldM (\exp el -> 
        return (AppE exp el)) name args
    return x

-- Generate a pattern that represents the program state 
statePattern :: [Declaration] -> Q [Pat]
statePattern varDecs = mapM toPat varDecs
    where toPat (GlobalVarDeclaration var _) = varToPat var

-- Evaluate a global variable declaration to TH representation
evalGlobalVarDeclaration :: Declaration -> Q Stmt
evalGlobalVarDeclaration (GlobalVarDeclaration (Variable n t) e) = do
    let name = namify n
    return $ LetS [ValD (VarP name) (NormalB e) []]

-- Evaluate a procedure to it's corresponding TH representation
evalProcedure :: StatePatterns -> [Pat] -> Declaration -> Q Dec
evalProcedure stPatterns globalArgs (Procedure n vs b) = do
    let name = namify n
    inputArgs <- mapM varToPat vs
    let pattern = TupP (globalArgs ++ inputArgs)
    body <- evalProcedureBody stPatterns b pattern
    return $ FunD name [Clause (globalArgs ++ inputArgs) body []]

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
evalProcedureBody :: StatePatterns -> [Statement] -> Pat -> Q Body
evalProcedureBody stPatterns ss pattern = do
    x <- concatMapM (evalStatement stPatterns pattern) ss
    return $ NormalB $ DoE $ x ++ [returnTup]
        where returnTup = NoBindS $ tupP2tupE pattern

-- Evaluate a statement from a janus program to it's corresponding TH representation
evalStatement :: StatePatterns -> Pat -> Statement -> Q [Stmt]
evalStatement _ _ (Assignement op lhss expr)          = evalAssignments op lhss expr
evalStatement stPatterns _ (Call (Identifier i) args) = evalFunctionCall stPatterns i args
evalStatement stPatterns p (If exp tb eb _)           = evalIf stPatterns p exp tb eb
evalStatement stPatterns p (LoopUntil  )
evalStatement _ _ _ = error "Only Assignment can be evaluated at the moment."

-- Evaluate assignment to a list of Identifiers (as defined in AST.hs)
evalAssignments :: String -> [LHS] -> Exp -> Q [Stmt]
evalAssignments op lhss expr = concatMapM (\lhs -> evalAssignment op lhs expr) lhss

-- Evaluate an assignment (as defined in AST.hs) to an equivalent TH representation. 
-- Assignment in this context refers to any operation that changes the value of one 
-- or more global variables in some way. 
evalAssignment :: String -> LHS -> Exp -> Q [Stmt]
evalAssignment op (LHSIdentifier n) lhs = do
    let f = (VarE . mkName) op
    let x = namify n
    op' <- [|(\(Operator fwd _) -> fwd)|]
    let fApp = AppE (AppE (AppE op' f) (VarE x)) lhs
    tmpN <- newName "tmp"
    return [letStmt (VarP tmpN) fApp, letStmt (VarP x) (VarE tmpN)]
evalAssignment _ _ _ = error "Only LHSIdentifier can be evaluated at the moment."

-- Evaluate a janus procedure call to it's corresponding TH representation
evalFunctionCall :: StatePatterns -> String -> [LHS] -> Q [Stmt]
evalFunctionCall stPatterns name args = do
    let returnP = TupP pattern
    tmpN <- newName "tmp"
    f <- foldM (\exp pat -> do
                    arg <- expFromVarP pat
                    return (AppE exp arg))
         ((VarE . mkName) name) pattern
    return [letStmt (VarP tmpN) f, letStmt returnP (VarE tmpN)] 
    where pattern = case Map.lookup (Identifier name) stPatterns of
                        (Just pat) -> pat
                        Nothing    -> error "call to unknown function" 

-- Evaluate a janus 'if' statement to it's corresponding TH representation
evalIf :: StatePatterns -> Pat -> Exp -> [Statement] -> [Statement] -> Q [Stmt]
evalIf stPatterns pattern g tb eb = do
    b1   <- evalBranch tb
    b2   <- evalBranch eb
    tmpN <- newName "tmp"
    let ifExp  = CondE g b1 b2
    let ifStmt = letStmt (VarP tmpN) ifExp
    return $ [ifStmt, letStmt pattern (VarE tmpN)]
    where evalBranch branch = do 
            stmts <- concatMapM (evalStatement stPatterns pattern) branch
            return $ DoE (stmts ++ [(NoBindS (tupP2tupE pattern))])

evalWhile :: StatePatterns -> Pat -> Exp -> [Statement] -> Q ([Stmt], Dec)
evalWhile stPatterns pattern guard body = undefined
    where 

-- *** HELPERS *** ---

-- Convert an identifier (as defined in AST.hs) to a TH name
namify :: Identifier -> Name
namify (Identifier n) = mkName n

-- Convert a variable (as defined in AST.hs) to a TH 
-- pattern representing that variable
varToPat :: Variable -> Q Pat
varToPat (Variable n t) = do
    let name = namify n
    return $ SigP (VarP name) t

-- Create a TH expression referencing a variable from a TH pattern referencing
-- a variable
expFromVarP :: Pat -> Q Exp
expFromVarP (SigP (VarP name) _) = return (VarE name) 

-- Convert a TH tuple pattern to a TH tuple expression
tupP2tupE :: Pat -> Exp
tupP2tupE (TupP pats) = TupE $ map (\(SigP (VarP name) _) -> VarE name) pats

-- Monadic version of concatmap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (return [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; return $ x++xs

-- Generate patterns for al functions in the janus program representing program state. These
-- are needed in order to generate patterns for calling functions, since the format of state
-- data changes when function arguments are introduced. 
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

-- Enumerate all procedure declarations in a janus program
getProcedures :: Program -> [Declaration]
getProcedures (Program xs) = filter filterProcs xs
    where filterProcs dec = case dec of 
                                Procedure _ _ _ -> True
                                otherwise       -> False

-- Enumerate all global variable declarations in a janus program
getVariableDecs :: Program -> [Declaration]
getVariableDecs (Program decs) = filter filterVars decs
    where filterVars dec  = case dec of 
                                GlobalVarDeclaration _ _ -> True
                                otherwise                -> False