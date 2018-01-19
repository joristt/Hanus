{-# LANGUAGE TemplateHaskell, ScopedTypeVariables #-} 

module Eval where

import AST
import StdLib.Operator
import StdLib.DefaultValue
import StdLib.ArrayIndexer

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

import System.IO.Unsafe

import qualified Debug.Trace as Debug

trace x = Debug.trace (show x) x

type Env = (Globals, Scope)
type Globals = Pat
type Scope = Pat

type EvalState = ([Stmt], [Dec], Env)

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
    return $ (map fst fdecs) ++ (concatMap snd fdecs)
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
    return (ValD (VarP name) (NormalB (defVal)) []) 

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
evalProcedure :: [Pat] -> Declaration -> Q (Dec, [Dec])
evalProcedure globalArgs (Procedure n vs b) = do
    let name = case n of 
                   (Identifier "main") -> nameId $ Identifier "hanus_main"
                   otherwise -> nameId n
    let inputArgs = map (VarP . (\(Variable (Identifier n) _) -> mkName n)) vs
    let pattern = TupP (globalArgs)
    body <- evalProcedureBody b (pattern, TupP (globalArgs ++ inputArgs))
    return (FunD name [Clause (globalArgs ++ inputArgs) (fst body) []], snd body)

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
evalProcedureBody :: [Statement] -> Env -> Q (Body, [Dec])
evalProcedureBody ss environment = do
    x <- foldM accResult (initR environment) ss
    return (NormalB $ DoE $ (frst x) ++ [returnTup], scnd x)
        where returnTup = NoBindS $ tupP2tupE (fst environment)

-- Evaluate a statement from a janus program to it's corresponding TH representation
evalStatement :: Env -> Statement -> Q EvalState
evalStatement env (Assignment direction op lhss expr) = evalAssignment env direction op lhss expr
evalStatement env (Call (Identifier i) args)          = evalFunctionCall env i args
evalStatement env (If exp tb eb _)                    = evalIf env exp tb eb
evalStatement env (LoopUntil from d l until)          = evalWhile env from until d l 
evalStatement env (LocalVarDeclaration var i stmts e) = do
  varPat <- varToPat var
  evalLocalVarDec env varPat i stmts e
evalStatement  _ _ = error "Statement not implementend"

-- Evaluate an assignment (as defined in AST.hs) to an equivalent TH representation. 
-- Assignment in this context refers to any operation that changes the value of one 
-- or more global variables in some way. 
evalAssignment :: Env -> Bool -> String -> [LHS] -> Exp -> Q EvalState
evalAssignment env direction op lhss exp = do
    let f = (VarE . mkName) op
    op' <- case direction of 
               False -> [|(\(Operator fwd _) -> fwd)|]
               True  -> [|(\(Operator _ bwd) -> bwd)|]
    let vf = AppE op' f
    vNames <- replicateM (length lhss) (newName "v")
    let valuesP = (TupP . map VarP) vNames
    let call = letStmt valuesP argsE
    restores <- concatMapM restoreStmt (zip vNames lhss)
    return (call:restores, [], env)
    where argsE = (TupE . map argE) lhss
          argE lhs = case lhs of
                       (LHSIdentifier ident) -> VarE $ nameId ident
                       (LHSArray lhs exp)    -> 
                         AppE (AppE ((VarE . mkName) "indexerGet") (argE lhs)) exp
                       (LHSField _ _) -> undefined
          restoreStmt (vname, lhs) = 
            case lhs of 
              (LHSIdentifier ident) -> return $ [letStmt (VarP $ nameId ident) (VarE vname)]
              (LHSArray lhs exp)    -> do
                tmpN <- newName "tmp"
                let res = letStmt (VarP tmpN) (AppE (AppE (AppE ((VarE . mkName) "indexerSet") (argE lhs)) exp) (VarE vname))
                return [res, letStmt (lhsP lhs) (VarE tmpN)]
          lhsP lhs = case lhs of
                       (LHSIdentifier ident) -> VarP $ nameId ident
                       otherwise             -> undefined


-- Evaluate a janus procedure call to it's corresponding TH representation
evalFunctionCall :: Env -> String -> [LHS] -> Q EvalState
evalFunctionCall env name args = do
    tmpN <- newName "tmp"
    f <- foldM (\exp pat -> do
                    arg <- expFromVarP pat
                    return (AppE exp arg))
         ((VarE . mkName) name) (pattern' ++ (map (\(LHSIdentifier (Identifier n)) -> VarP (mkName n)) args))
    return ([letStmt (VarP tmpN) f, letStmt (fst env) (VarE tmpN)], [], env)
    where pattern' = (unwrapTupleP (fst env))
                                    

evalFunctionCallWithName :: Env -> Name -> [LHS] -> Q EvalState
evalFunctionCallWithName env name args = do
    tmpN <- newName "tmp"
    f <- foldM (\exp pat -> do
                    arg <- expFromVarP pat
                    return (AppE exp arg))
         (VarE name) pattern'
    x <- argsE
    f' <- foldM (\x a -> return $ AppE x a) f x
    return ([letStmt (VarP tmpN) f', letStmt (fst env) (VarE tmpN)], [], env)
    where pattern' = (unwrapTupleP (fst env))
          argsE = do
            x <- mapM (\(LHSIdentifier (Identifier n)) -> argE (mkName n)) args
            return x
            where argE   n = do
                    x <- argSet n
                    return $ TupE [argGet n, x]
                  argGet n = LamE [fst env] (VarE n)
                  argSet n = do
                      vName <- newName "v"
                      return $ LamE [VarP vName, fst env]
                          ((TupE . map VarE) $ replace n vName $ (map (\(VarP n) -> n) . unwrapTupleP) (fst env))

-- Evaluate a janus 'if' statement to it's corresponding TH representation
evalIf :: Env -> Exp -> [Statement] -> [Statement] -> Q EvalState
evalIf env g tb eb = do
    b1   <- evalBranch tb
    b2   <- evalBranch eb
    tmpN <- newName "tmp"
    let ifExp  = CondE g b1 b2
    let ifStmt = letStmt (VarP tmpN) ifExp
    return ([ifStmt, letStmt (snd env) (VarE tmpN)], [], env)
    where evalBranch branch = do 
            stmts <- foldM accResult (initR env) branch
            return $ DoE ((frst stmts) ++ [(NoBindS (tupP2tupE (snd env)))])

evalIfErr :: Env -> Exp -> EvalState -> [Stmt] -> Q EvalState
evalIfErr env g tb eb = do
    b1     <- evalBranch tb
    let b2  = DoE eb
    tmpN   <- newName "tmp"
    let ifExp  = CondE g b1 b2
    let ifStmt = letStmt (VarP tmpN) ifExp
    return ([ifStmt, letStmt (snd env) (VarE tmpN)], [], env)
    where evalBranch stmts = do 
            return $ DoE ((frst stmts) ++ [(NoBindS (tupP2tupE (snd env)))])

evalSingleBranchIf :: Env -> Exp -> [Stmt] -> Q EvalState
evalSingleBranchIf env g eb = do
    b1   <- evalBranch ([], [], env)
    b2   <- evalBranch (eb, [], env)
    tmpN <- newName "tmp"
    let ifExp  = CondE g b1 b2
    let ifStmt = letStmt (VarP tmpN) ifExp
    return ([ifStmt, letStmt (snd env) (VarE tmpN)], [], env)
    where evalBranch stmts = do 
            return $ DoE ((frst stmts) ++ [(NoBindS (tupP2tupE (snd env)))])

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
evalWhile :: Env -> Exp -> Exp -> [Statement] -> [Statement] -> Q EvalState
evalWhile env@(TupP globals, scope) fromGuard untilGuard doStatements loopStatements = do
    whileProcName <- newName "while"

    whileProcCall <- evalFunctionCallWithName env whileProcName [] -- the empty list here shouldn't be empty.
    -- The while loop can only be evaluated if fromGuard is true the first time (and *only* the first time).
    err <- runQ [|error "From-guard in while loop was not true upon first evaluation."|]
    -- The err will be thrown in a do block that should return a value, so we actually
    -- have to return a bogus value after we throw the error in order to please Haskell.
    let errStmt = NoBindS $ AppE err (tupP2tupE (snd env))
    whileIf       <- evalIfErr env fromGuard whileProcCall [errStmt]

    err <- runQ [|error "From-guard in while loop was true at some point *after* the first iteration."|]
    -- The err will be thrown in a do block that should return a value, so we actually
    -- have to return a bogus value after we throw the error in order to please Haskell.
    let errStmt = NoBindS $ AppE err (tupP2tupE (snd env))
    whileProcLoopIf       <- evalIfErr env (AppE (VarE (mkName "not")) fromGuard) whileProcCall [errStmt]
    loopStmts             <- evalStmts loopStatements
    let whileProcLoopBlock = loopStmts ++ (frst whileProcLoopIf)

    whileProcDoIf       <- evalSingleBranchIf env untilGuard whileProcLoopBlock
    doStmts             <- evalStmts doStatements
    let whileProcBlock = doStmts ++ (frst whileProcDoIf)

    whileProcDec      <- evalProcedure2 globals whileProcName [] whileProcBlock -- the empty list here shouldn't be empty.

    return (frst whileIf, [whileProcDec], env)

    where evalStmts stmts = do
              stmts <- foldM accResult (initR env) stmts
              return $ (frst stmts)

evalLocalVarDec :: Env -> Pat -> Exp -> [Statement] -> Exp -> Q EvalState
evalLocalVarDec env var init body exit = do
    tmpN <- newName "tmp"
    let env' = (fst env, (TupP . (:) var. unwrapTupleP . snd) env)
    stmts <- foldM accResult (initR env') body
    let body' = DoE ((frst stmts) ++ [(NoBindS (tupP2tupE $ snd env))])
    let asg = LetE [ValD var (NormalB init) []] body'
    return ([letStmt (VarP tmpN) asg, letStmt (snd env) (VarE tmpN)], [], env)

-- *** HELPERS *** ---

nameId :: Identifier -> Name
nameId (Identifier n) = mkName n

-- Convert a variable (as defined in AST.hs) to a TH 
-- pattern representing that variable
varToPat :: Variable -> Q Pat
varToPat (Variable n t) = do
    let name = nameId n
    return $ VarP name

-- Create a TH expression referencing a variable from a TH pattern referencing
-- a variable
expFromVarP :: Pat -> Q Exp
expFromVarP (VarP name) = return (VarE name) 

-- Convert a TH tuple pattern to a TH tuple expression
tupP2tupE :: Pat -> Exp
tupP2tupE (TupP pats) = TupE $ map (\(VarP name) -> VarE name) pats

unwrapTupleP :: Pat -> [Pat]
unwrapTupleP (TupP xs) = xs

-- Monadic version of concatmap
concatMapM :: Monad m => (a -> m [b]) -> [a] -> m [b]
concatMapM op = foldr f (return [])
    where f x xs = do x <- op x; if null x then xs else do xs <- xs; return $ x++xs

accResult :: EvalState -> Statement ->  Q EvalState
accResult r stmt = do
  eval <- evalStatement (thrd r) stmt
  return (frst r ++ frst eval, scnd r ++ scnd eval, thrd eval)

initR :: Env -> EvalState
initR env = ([], [], env) 

frst :: (a, b, c) -> a
frst (x, _, _) = x

scnd :: (a, b, c) -> b
scnd (_, y, _) = y

thrd :: (a, b, c) -> c
thrd (_, _, z) = z

removeVar :: Pat -> Env -> Env
removeVar var env = (fst env, TupP $ List.delete var $ (unwrapTupleP . snd) env)

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

