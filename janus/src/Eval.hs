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
import qualified Data.List as List
import Data.Maybe

import System.IO.Unsafe

import Debug.Trace

--import qualified Debug.Trace as Debug

--trace x = trace (show x) x

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
    args <- mapM (\(GlobalVarDeclaration (Variable (Identifier x) t)) -> 
        return (SigE ((VarE . mkName) x) t)) decs
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
    let inputArgs = map (\(Variable (Identifier n) t) -> SigP (VarP (mkName n)) t) vs
    let pattern = TupP globalArgs
    body <- evalProcedureBody b (pattern, TupP (globalArgs ++ inputArgs))
    return (FunD name [Clause (globalArgs ++ inputArgs) (fst body) []], snd body)

-- Evaluate a procedure to it's corresponding TH representation
evalProcedure2 :: Name -> Pat -> [Stmt] -> Q Dec
evalProcedure2 name scopeTup@(TupP scope) stmts = do
    body <- evalProcedureBody2 stmts scopeTup
    return $ FunD name [Clause scope body []]

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
evalProcedureBody2 :: [Stmt] -> Pat -> Q Body
evalProcedureBody2 stmts entireScope = do
    return $ NormalB $ DoE $ stmts ++ [returnTup]
        where returnTup = NoBindS $ tupP2tupE entireScope

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
evalProcedureBody :: [Statement] -> Env -> Q (Body, [Dec])
evalProcedureBody ss env = do
    x <- foldM accResult (initR env) ss
    return (NormalB $ DoE $ (frst x) ++ [returnTup], scnd x)
        where returnTup = NoBindS $ tupP2tupE (snd env)

-- Evaluate a statement from a janus program to it's corresponding TH representation
evalStatement :: Env -> Statement -> Q EvalState
evalStatement env (Assignment direction op lhss expr) = evalAssignment env direction op lhss expr
evalStatement env (Call (Identifier i) args)          = evalFunctionCall env i args
evalStatement env (If exp tb eb _)                    = evalIf env exp tb eb
evalStatement env (LoopUntil from d l until)          = evalWhile env from until d l 
evalStatement env (Log   lhss)                        = evalLog env lhss
evalStatement env (LocalVarDeclaration var i stmts e) = do
    varPat <- varToPat var
    evalLocalVarDec env varPat i stmts e
evalStatement  _ _ = error "Statement not implementend"

-- If input is ["x"] then output is [", ", "x", " : ", "<value_of_x>"]
evalLogUpdate :: [LHS] -> [Exp]
evalLogUpdate [] = []
evalLogUpdate (x:xs) = do
    let name       = lhsToString x
    let nameExp    = LitE $ StringL name
    let sepExp     = LitE $ StringL " : "
    let commaExp   = LitE $ StringL ", "
    let valExp     = AppE (toE "show") (toE name)
    [commaExp, nameExp, sepExp, valExp] ++ evalLogUpdate xs
    --msgExp ++ evalLogUpdate xs
        where lhsToString (LHSIdentifier (Identifier name)) = name
              --a `append` b = AppE (AppE (VarE (mkName "++")) b) a

evalLog :: Env -> [LHS] -> Q EvalState
evalLog env xs = do
    -- We take the tail because the first Exp is a separator.
    let logUpdateStmts = ListE $ tail $ evalLogUpdate xs
    tmpN <- newName "tmp"
    let concatedList = ListE [(AppE (toE "concat") logUpdateStmts)]
    let newLog = AppE (AppE (toE "++") (toE logName)) concatedList
    let let1 = letStmt (VarP tmpN) newLog
    let let2 = letStmt (toP logName) (VarE tmpN)
    return ([let1, let2], [], env)

-- Evaluate an assignment (as defined in AST.hs) to an equivalent TH representation. 
-- Assignment in this context refers to any operation that changes the value of one 
-- or more global variables in some way. 
evalAssignment :: Env -> Bool -> String -> [LHS] -> Exp -> Q EvalState
evalAssignment env direction op lhss exp = do
    let f = (VarE . mkName) op
    op' <- case direction of 
               False -> [|(\(Operator fwd _) -> fwd)|]
               True  -> [|(\(Operator _ bwd) -> bwd)|]
    let fApp = AppE (AppE (AppE op' f) arg) exp
    tmpN <- newName "tmp"
    return ([letStmt (VarP tmpN) fApp, letStmt pat (VarE tmpN)], [], env)
      where arg = case lhss of
                    [(LHSIdentifier ident)] -> VarE $ nameId ident
                    lst@(x:xs) -> TupE $ map (\(LHSIdentifier ident) -> (VarE . nameId) ident) lst
                    otherwise  -> error "Only LHSIdentifier can be evaluated currently"
            pat = case lhss of
                    [(LHSIdentifier ident)] -> VarP $ nameId ident
                    lst@(x:xs) -> TupP $ map (\(LHSIdentifier ident) -> (VarP . nameId) ident) lst
                    otherwise  -> error "Only LHSIdentifier can be evaluated currently"

-- Evaluate a janus procedure call to it's corresponding TH representation
evalFunctionCall :: Env -> String -> [LHS] -> Q EvalState
evalFunctionCall env@(TupP globalsList, _) name args = do
    tmpN <- newName "tmp"
    f <- foldM (\exp pat -> do
                    arg <- expFromVarP pat
                    return (AppE exp arg))
         ((VarE . mkName) name) (pattern' ++ argsPats)
    return ([letStmt (VarP tmpN) f, letStmt returnPat (VarE tmpN)], [], env)
    where pattern' = (unwrapTupleP (fst env))
          argsPats = map (\(LHSIdentifier (Identifier n)) -> VarP (mkName n)) args
          returnPat = TupP (globalsList ++ argsPats)

evalFunctionCallWithName :: Env -> Name -> Pat -> Q EvalState
evalFunctionCallWithName env name (TupP args) = do
    tmpN <- newName "tmp"
    f <- foldM (\exp pat -> do
                    arg <- expFromVarP pat
                    return (AppE exp arg))
         (VarE name)
         args
    return ([letStmt (VarP tmpN) f, letStmt (fst env) (VarE tmpN)], [], env)

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
    whileProcCall <- evalFunctionCallWithName (scope, scope) whileProcName scope -- the empty list here shouldn't be empty.
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

    whileProcDec      <- evalProcedure2 whileProcName scope whileProcBlock -- the empty list here shouldn't be empty.

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
    return ([letStmt (VarP tmpN) asg, letStmt (snd env) (VarE tmpN)], scnd stmts, env)

-- *** HELPERS *** ---

-- generate a let statement from pattern and expression of the form
-- let *pat* = *exp* to be used in do expressions
letStmt :: Pat -> Exp -> Stmt
letStmt pattern exp = LetS [ValD pattern (NormalB exp) []]

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
expFromVarP p = return $ pToE p

-- Convert a TH tuple pattern to a TH tuple expression
tupP2tupE :: Pat -> Exp
tupP2tupE (TupP pats) = TupE $ map pToE pats

pToE :: Pat -> Exp
pToE (VarP name)          = VarE name
pToE (SigP (VarP name) t) = SigE (VarE name) t

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

toE :: String -> Exp
toE s = VarE (mkName s)

toP :: String -> Pat
toP s = VarP (mkName s)

-- "_log" is a special variable that is never used by the user but
-- is used by the generated Haskell code to store log messages. Since
-- Haskell evaluates in a bottom-up fashion, sequential log statements
-- will be printed in reversed order. Therefore, all log messages will
-- first be collected in _log, and then _log will be printed when the
-- program has terminated.
logDecl :: Declaration
logDecl = GlobalVarDeclaration (Variable (Identifier logName) logType)
    where logType = ConT (mkName "[String]")

logName :: String
logName = "_log"

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
getVariableDecs (Program decs) = logDecl : (filter filterVars decs)
    where filterVars dec  = case dec of 
                                GlobalVarDeclaration _ -> True
                                otherwise              -> False
