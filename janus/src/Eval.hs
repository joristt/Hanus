{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-} 

module Eval where

import AST
import StdLib.Operator
import StdLib.DefaultValue

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import System.IO.Unsafe

import qualified Debug.Trace as Debug

trace x = Debug.trace (show x) x

type Env = (Globals, Scope)
type Globals = Pat
type Scope = Pat

-- The purpose of this function is to generate a TH object that represents a program equivalent 
-- to the given janus program. The resulting TH object can then be spliced and run from within 
-- another file  
evalProgram :: Program -> Q [Dec]
evalProgram p = do  
    let nameFwd = mkName "run"
    nameBwd <- newName "run_bwd"
    -- generate program entry point ('run' function)
    x  <- entry nameFwd nameBwd
    -- generate pattern for program state
    decs <- evalProgramT p
    return $ x:decs
    where -- generates the program entry point
          entry fwd bwd = do
              fcall <- getMain globalVars
              binds <- vdecs
              stTup <- statePattern globalVars
              let body = DoE (binds:[NoBindS fcall])
              return (FunD fwd [Clause [] (NormalB body) []])
          globalVars = getVariableDecs p
          -- Let bindigs for variable declarations
          vdecs = do
              decs <- mapM genDec globalVars
              return (LetS decs)

-- Evaluate a program, but do not generate and entry point. 
-- Used for testing purposes, hence the suffix 'T'
evalProgramT :: Program -> Q [Dec]
evalProgramT p@(Program decls) = do
    -- generate pattern for program state
    pt <- statePattern globalVars
    fdecs <- mapM (evalProcedure pt) procedures
    return fdecs
    where procedures = getProcedures p
          globalVars = getVariableDecs p 

-- generate a let statement from pattern and expression of the form
-- let *pat* = *exp* to be used in do expressions
letStmt :: Pat -> Exp -> Stmt
letStmt pattern exp = LetS [ValD pattern (NormalB exp) []]

-- Generate variable declarations for global variables
genDec :: Declaration -> Q Dec
genDec (GlobalVarDeclaration (Variable ident t)) = do 
    let name = nameId ident
    defVal <- runQ [|defaultValue|]
    return (ValD (SigP (VarP name) t) (NormalB (defVal)) []) 

-- Generate an expression that calls the main function with all global 
-- variables as state
getMain :: [Declaration] -> Q Exp
getMain decs = do 
    name <- [e|hanus_main|]
    args <- mapM (\(GlobalVarDeclaration (Variable (Identifier x) _)) -> 
        return ((VarE . mkName) x)) decs
    x <- foldM (\exp el -> 
        return (AppE exp el)) name args
    return x

-- Generate a pattern that represents the program state 
statePattern :: [Declaration] -> Q [Pat]
statePattern varDecs = mapM toPat varDecs
    where toPat (GlobalVarDeclaration var) = varToPat var

-- Evaluate a procedure to it's corresponding TH representation
evalProcedure :: [Pat] -> Declaration -> Q Dec
evalProcedure globalArgs (Procedure n vs b) = do
    let name = case n of 
                   (Identifier "main") -> nameId $ Identifier "hanus_main"
                   otherwise -> nameId n
    let inputArgs = map (VarP . (\(Variable (Identifier n) _) -> mkName n)) vs
    let pattern = TupP (globalArgs)
    body <- evalProcedureBody b (pattern, TupP (globalArgs ++ inputArgs))
    return $ FunD name [Clause (globalArgs ++ inputArgs) body []]

-- Evaluate a procedure to it's corresponding TH representation
evalProcedure2 :: [Pat] -> Name -> [Variable] -> [Stmt] -> Q Dec
evalProcedure2 globalArgs name vs stmts = do
    let inputArgs = map (VarP . (\(Variable (Identifier n) _) -> mkName n)) vs
    let pattern = TupP (globalArgs)
    body <- evalProcedureBody2 stmts (pattern, TupP (globalArgs ++ inputArgs))
    return $ FunD name [Clause (globalArgs ++ inputArgs) body []]

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
evalProcedureBody2 :: [Stmt] -> Env -> Q Body
evalProcedureBody2 stmts environment = do
    return $ NormalB $ DoE $ stmts ++ [returnTup]
        where returnTup = NoBindS $ tupP2tupE (fst environment)

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
evalProcedureBody :: [Statement] -> Env -> Q Body
evalProcedureBody ss environment = do
    x <- concatMapM (evalStatement environment) ss
    return $ NormalB $ DoE $ x ++ [returnTup]
        where returnTup = NoBindS $ tupP2tupE (fst environment)

-- Evaluate a statement from a janus program to it's corresponding TH representation
evalStatement :: Env -> Statement -> Q [Stmt]
evalStatement _   (Assignment direction op lhss expr) = evalAssignment direction op lhss expr
evalStatement env (Call (Identifier i) args)          = evalFunctionCall (fst env) i args
evalStatement env (If exp tb eb _)                    = evalIf env exp tb eb
evalStatement env (LoopUntil from d l until)          = undefined 
evalStatement env (LocalVarDeclaration var i stmts e) = do
  varPat <- varToPat var
  evalLocalVarDec env varPat i stmts e
evalStatement  _ _ = error "Statement not implementend"

-- Evaluate an assignment (as defined in AST.hs) to an equivalent TH representation. 
-- Assignment in this context refers to any operation that changes the value of one 
-- or more global variables in some way. 
evalAssignment :: Bool -> String -> [LHS] -> Exp -> Q [Stmt]
evalAssignment direction op lhss exp = do
    let f = (VarE . mkName) op
    op' <- case direction of 
               False -> [|(\(Operator fwd _) -> fwd)|]
               True  -> [|(\(Operator _ bwd) -> bwd)|]
    let fApp = AppE (AppE (AppE op' f) arg) exp
    tmpN <- newName "tmp"
    return [letStmt (VarP tmpN) fApp, letStmt pat (VarE tmpN)]
      where arg = case lhss of
                    [(LHSIdentifier ident)] -> VarE $ nameId ident
                    lst@(x:xs) -> TupE $ map (\(LHSIdentifier ident) -> (VarE . nameId) ident) lst
                    otherwise  -> error "Only LHSIdentifier can be evaluated currently"
            pat = case lhss of
                    [(LHSIdentifier ident)] -> VarP $ nameId ident
                    lst@(x:xs) -> TupP $ map (\(LHSIdentifier ident) -> (VarP . nameId) ident) lst
                    otherwise  -> error "Only LHSIdentifier can be evaluated currently"

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
                                    

evalFunctionCallWithName :: Pat -> Name -> [LHS] -> Q [Stmt]
evalFunctionCallWithName pattern name args = do
    tmpN <- newName "tmp"
    f <- foldM (\exp pat -> do
                    arg <- expFromVarP pat
                    return (AppE exp arg))
         (VarE name) pattern'
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
evalIf :: Env -> Exp -> [Statement] -> [Statement] -> Q [Stmt]
evalIf env g tb eb = do
    b1   <- evalBranch tb
    b2   <- evalBranch eb
    tmpN <- newName "tmp"
    let ifExp  = CondE g b1 b2
    let ifStmt = letStmt (VarP tmpN) ifExp
    return $ [ifStmt, letStmt (snd env) (VarE tmpN)]
    where evalBranch branch = do 
            stmts <- concatMapM (evalStatement env) branch
            return $ DoE (stmts ++ [(NoBindS (tupP2tupE (snd env)))])

evalIf2 :: Env -> Exp -> [Stmt] -> [Stmt] -> Q [Stmt]
evalIf2 env g tb eb = do
    b1   <- evalBranch tb
    b2   <- evalBranch eb
    tmpN <- newName "tmp"
    let ifExp  = CondE g b1 b2
    let ifStmt = letStmt (VarP tmpN) ifExp
    return $ [ifStmt, letStmt (snd env) (VarE tmpN)]
    where evalBranch stmts = do 
            return $ DoE (stmts ++ [(NoBindS (tupP2tupE (snd env)))])

{- Flow of a loop is: 
      fromGuard True -> doStmts -> untilGuard True -> loop successfully terminates
          |                ^            |
          v                |            |
        False              |            |
          |                |            |
          v                |            v
        error              |          False -> loopStmts -> fromGuard True -> error.
                           |                                     |
                           |                                     v
                           ----------------------------------- False
-}
evalWhile :: Env -> Exp -> Exp -> [Statement] -> [Statement] -> Q ([Stmt], Dec)
evalWhile pTup@(TupP patList, scope) fromGuard untilGuard doStatements loopStatements = do
    whileProcName <- newName "while"

    whileProcCall <- evalFunctionCallWithName scope whileProcName [] -- the empty list here shouldn't be empty.
    -- The while loop can only be evaluated if fromGuard is true the first time (and *only* the first time).
    err           <- runQ [|error "From-guard in while loop was not true upon first evaluation."|]
    whileIf       <- evalIf2 pTup fromGuard whileProcCall [NoBindS err]

    err                   <- runQ [|error "From-guard in while loop was true after at least one iteration."|]
    whileProcLoopIf       <- evalIf2 pTup fromGuard [NoBindS err] whileProcCall
    loopStmts             <- evalStmts loopStatements
    let whileProcLoopBlock = loopStmts ++ whileProcLoopIf

    whileProcDoIf       <- evalIf2 pTup untilGuard [] whileProcLoopBlock
    doStmts             <- evalStmts doStatements
    let whileProcDoBlock = doStmts ++ whileProcDoIf

    let whileProcBlock = whileProcDoBlock ++ whileProcLoopBlock
    whileProcDec      <- evalProcedure2 patList whileProcName [] whileProcBlock -- the empty list here shouldn't be empty.
    
    return (whileIf, whileProcDec)

    where evalStmts stmts = do
              stmts <- concatMapM (evalStatement pTup) stmts
              return $ stmts ++ [(NoBindS (tupP2tupE scope))]

evalLocalVarDec :: Env -> Pat -> Exp -> [Statement] -> Exp -> Q [Stmt]
evalLocalVarDec env var init body exit = do
    tmpN <- newName "tmp"
    stmts <- concatMapM (evalStatement env) body
    let body' = DoE (stmts ++ [(NoBindS (tupP2tupE (snd env)))])
    let asg = LetE [ValD var (NormalB init) []] body'
    return $ [letStmt (VarP tmpN) asg, letStmt (snd env) (VarE tmpN)]

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
                                GlobalVarDeclaration _ -> True
                                otherwise              -> False

-- Gets all the names of the global variables in the program
variableNames :: Program -> [String]
variableNames program = map varToName (getVariableDecs program)
    where varToName (GlobalVarDeclaration (Variable (Identifier n) _)) = n

