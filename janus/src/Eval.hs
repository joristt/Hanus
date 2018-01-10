{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-} 

module Eval where

import AST
import StdLib.Operator

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import Control.Monad
import Control.Monad.State

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import qualified Debug.Trace as Debug

trace x = Debug.trace (show x) x

-- Example program for debugging purposes
globvartype = ConT $ mkName "Int"
globvar1 = GlobalVarDeclaration (Variable (Identifier "glob_var1") globvartype) (LitE (IntegerL 10))
globvar2 = GlobalVarDeclaration (Variable (Identifier "glob_var2") globvartype) (LitE (IntegerL 1))
proc1 = Procedure (Identifier "main") [] [Assignement "+=" [LHSIdentifier (Identifier "glob_var2")] (LitE (IntegerL 10)), Call (Identifier "substract") [LHSIdentifier (Identifier "glob_var1")]]
proc2 = Procedure (Identifier "substract") [Variable (Identifier "arg1") globvartype] [(Assignement "-=" [LHSIdentifier (Identifier "glob_var1")] ((VarE . mkName) "glob_var2"))]
p = Program [globvar1, globvar2, proc1, proc2]

-- Generates program entry points for running the program both forward and backward, as well as
-- function declarations for all procedures in the janus program. 
--
-- The purpose of this function is to generate a TH object that represents a program equivalent 
-- to the given janus program. The resulting TH object can then be spliced and run from within 
-- another file  
evalProgram :: Program -> Q [Dec]
evalProgram p@(Program decls) = do  
    nameFwd <- newName "run"
    nameBwd <- newName "run_bwd"
    -- generate program entry point ('run' function)
    x  <- entry nameFwd nameBwd
    -- generate pattern for program state
    pt <- statePattern globalVars
    fdecs <- mapM (evalProcedure pt) procedures
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
    let name = trace $ nameId ident
    return (ValD (SigP (VarP name) t) (NormalB exp) []) 

-- Generate an expression that calls the main function with all global 
-- variables as state
getMain :: [Declaration] -> Q Exp
getMain decs = do 
    name <- [e|main|]
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
    let name = nameId n
    return $ LetS [ValD (VarP name) (NormalB e) []]

-- Evaluate a procedure to it's corresponding TH representation
evalProcedure :: [Pat] -> Declaration -> Q Dec
evalProcedure globalArgs (Procedure n vs b) = do
    let name = nameId n
    inputArgs <- mapM varToPat vs
    let pattern = TupP (globalArgs ++ inputArgs)
    body <- evalProcedureBody b pattern
    return $ FunD name [Clause (globalArgs ++ inputArgs) body []]

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
evalProcedureBody :: [Statement] -> Pat -> Q Body
evalProcedureBody ss pattern = do
    x <- concatMapM (evalStatement pattern) ss
    return $ NormalB $ DoE $ x ++ [returnTup]
        where returnTup = NoBindS $ tupP2tupE pattern

-- Evaluate a statement from a janus program to it's corresponding TH representation
evalStatement :: Pat -> Statement -> Q [Stmt]
evalStatement _ (Assignement op lhss expr)          = evalAssignments op lhss expr
evalStatement p (Call (Identifier i) args)          = evalFunctionCall p i args
evalStatement p (If exp tb eb _)                    = evalIf p exp tb eb
evalStatement p (LoopUntil from d l until)          = undefined
evalStatement  _ _ = error "Statement not implementend"

-- Evaluate assignment to a list of Identifiers (as defined in AST.hs)
evalAssignments :: String -> [LHS] -> Exp -> Q [Stmt]
evalAssignments op lhss expr = concatMapM (\lhs -> evalAssignment op lhs expr) lhss

-- Evaluate an assignment (as defined in AST.hs) to an equivalent TH representation. 
-- Assignment in this context refers to any operation that changes the value of one 
-- or more global variables in some way. 
evalAssignment :: String -> LHS -> Exp -> Q [Stmt]
evalAssignment op (LHSIdentifier n) lhs = do
    let f = (VarE . mkName) op
    let x = nameId n
    op' <- [|(\(Operator fwd _) -> fwd)|]
    let fApp = AppE (AppE (AppE op' f) (VarE x)) lhs
    tmpN <- newName "tmp"
    return [letStmt (VarP tmpN) fApp, letStmt (VarP x) (VarE tmpN)]
evalAssignment _ _ _ = error "Only LHSIdentifier can be evaluated at the moment."

-- Evaluate a janus procedure call to it's corresponding TH representation
evalFunctionCall :: Pat -> String -> [LHS] -> Q [Stmt]
evalFunctionCall pattern name args = do
    tmpN <- newName "tmp"
    f <- foldM (\exp pat -> do
                    arg <- expFromVarP pat
                    return (AppE exp arg))
         ((VarE . mkName) name) pattern'
    x <- argsE
    f' <- foldM (\x a -> return $ AppE x a) f x
    return [letStmt (VarP tmpN) f', letStmt pattern (VarE tmpN)]
    where pattern' = (unwrapTupleP pattern)
          argsE = do
            x <- mapM (\(LHSIdentifier (Identifier n)) -> argE (mkName n)) args
            return x
            where argE   n = do
                    x <- argSet n
                    return $ TupE [argGet n, x]
                  argGet n = LamE [pattern] (VarE n)
                  argSet n = do
                      vName <- newName "v"
                      return $ LamE [VarP vName, pattern]
                          ((TupE . map VarE) $ replace n vName $ (map (\(SigP (VarP n) _) -> n) . unwrapTupleP) pattern)
                                    

-- Evaluate a janus 'if' statement to it's corresponding TH representation
evalIf :: Pat -> Exp -> [Statement] -> [Statement] -> Q [Stmt]
evalIf pattern g tb eb = do
    b1   <- evalBranch tb
    b2   <- evalBranch eb
    tmpN <- newName "tmp"
    let ifExp  = CondE g b1 b2
    let ifStmt = letStmt (VarP tmpN) ifExp
    return $ [ifStmt, letStmt pattern (VarE tmpN)]
    where evalBranch branch = do 
            stmts <- concatMapM (evalStatement pattern) branch
            return $ DoE (stmts ++ [(NoBindS (tupP2tupE pattern))])

evalWhile :: Pat -> Exp -> [Statement] -> Q ([Stmt], Dec)
evalWhile pattern guard body = undefined

-- *** HELPERS *** ---

nameId :: Identifier -> Name
nameId (Identifier n) = mkName n

-- Convert a variable (as defined in AST.hs) to a TH 
-- pattern representing that variable
varToPat :: Variable -> Q Pat
varToPat (Variable n t) = do
    let name = nameId n
    return $ SigP (VarP name) t

-- Create a TH expression referencing a variable from a TH pattern referencing
-- a variable
expFromVarP :: Pat -> Q Exp
expFromVarP (SigP (VarP name) _) = return (VarE name) 

-- Convert a TH tuple pattern to a TH tuple expression
tupP2tupE :: Pat -> Exp
tupP2tupE (TupP pats) = TupE $ map (\(SigP (VarP name) _) -> VarE name) pats

unwrapTupleP :: Pat -> [Pat]
unwrapTupleP (TupP xs) = xs

-- Monadic version of concatmap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (return [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; return $ x++xs

-- Enumerate all procedure declarations in a janus program
getProcedures :: Program -> [Declaration]
getProcedures (Program xs) = filter filterProcs xs
    where filterProcs dec = case dec of 
                                Procedure _ _ _ -> True
                                otherwise       -> False

-- Replaces the first occurence of an item in a list
replace :: Eq a => a -> a -> [a] -> [a]
replace _ _ [] = []
replace a b (x:xs) | a == x    = b:xs
                   | otherwise = x:(replace a b xs)

-- Enumerate all global variable declarations in a janus program
getVariableDecs :: Program -> [Declaration]
getVariableDecs (Program decs) = filter filterVars decs
    where filterVars dec  = case dec of 
                                GlobalVarDeclaration _ _ -> True
                                otherwise                -> False

-- Gets all the names of the global variables in the program
variableNames :: Program -> [String]
variableNames program = map varToName (getVariableDecs program)
    where varToName (GlobalVarDeclaration (Variable (Identifier n) _) _) = n

