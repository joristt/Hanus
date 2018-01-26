{-# LANGUAGE TemplateHaskell, ScopedTypeVariables, BangPatterns #-} 

module Eval where

import AST
import ReverseAST
import StdLib.Operator
import StdLib.DefaultValue
import StdLib.ArrayIndexer
import StdLib.FieldIndexer

import Language.Haskell.TH.Syntax
import Language.Haskell.TH

import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map
import qualified Data.List as List
import Data.Maybe

import System.IO.Unsafe

import qualified Debug.Trace as Debug

type Env = (Globals, Scope)
type Globals = Pat
type Scope = Pat

type EvalState = ([Stmt], [Dec], Env)

-- The purpose of this function is to generate a TH object that represents a program equivalent 
-- to the given janus program. The resulting TH object can then be spliced and run from within 
-- another file
evalProgram :: Program -> Q [Dec]
evalProgram p = do
    throwExceptionIfEvalImpossible
    let nameFwd = mkName "run"
    -- generate program entry point ('run' function)
    x  <- entry nameFwd
    -- generate pattern for program state
    decs <- evalProgramT p
    return $ x:decs
    where -- generates the program entry point
          entry fwd = do
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
    throwExceptionIfEvalImpossible
    -- generate pattern for program state
    pt <- statePattern globalVars
    dc <- concatMapM (evalProcedure pt) procedures
    --idxs <- getIndexers globalVars
    return (dc)
    where procedures = getProcedures p
          globalVars = getVariableDecs p 

-- generate a let statement from pattern and expression of the form
-- let *pat* = *exp* to be used in do expressions
letStmt :: Pat -> Exp -> Stmt
letStmt pattern exp = LetS [ValD pattern (NormalB exp) []]

throwExceptionIfEvalImpossible :: Q ()
throwExceptionIfEvalImpossible = do
    let requiredExts = [ScopedTypeVariables]
    scopedTypeVarsOn <- mapM isExtEnabled requiredExts >>= return . and
    if not scopedTypeVarsOn then
        error "The ScopedTypeVariables extension should be enabled for Hanus to work."
    else return ()

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

-- Returns a list with:
--   * the actual proc declaration
--   * the inverse proc declaration
--   * (possibly) additional declarations that were generated as helper
--     declarations to make the actual procs work.
evalProcedure :: [Pat] -> Declaration -> Q ([Dec])
evalProcedure globalArgs p@(Procedure (Identifier n) vs b) = do
    let p' = Procedure (Identifier (invert n)) vs (reverseBlock b)
    pDecl  <- actualEvalProcedure globalArgs $ p
    pDecl' <- actualEvalProcedure globalArgs $ p'
    return [pDecl, pDecl']

-- Evaluate a procedure to it's corresponding TH representation
actualEvalProcedure :: [Pat] -> Declaration -> Q Dec
actualEvalProcedure globalArgs (Procedure n vs b) = do
    let name = case n of 
                   (Identifier "main") -> nameId $ Identifier "hanus_main"
                   otherwise -> nameId n
    let inputArgs = map (\(Variable (Identifier n) t) -> SigP (VarP (mkName n)) t) vs
    let pattern = TupP globalArgs
    body <- evalProcedureBody b (pattern, TupP (globalArgs ++ inputArgs))
    return $ FunD name [Clause (globalArgs ++ inputArgs) (fst body) (snd body)]

-- Evaluate a procedure to it's corresponding TH representation
-- This function is specifically used as a part of evalWhile.
actualEvalProcedure' :: Name -> Pat -> [Stmt] -> Q Dec
actualEvalProcedure' name scopeTup@(TupP scope) stmts = do
    body <- evalProcedureBody' stmts scopeTup
    return $ FunD name [Clause scope body []]

-- Evaluates a procedure body (== Block (note that type Block = [Statement]))
-- This function is specifically used as a part of evalWhile.
evalProcedureBody' :: [Stmt] -> Pat -> Q Body
evalProcedureBody' stmts entireScope = do
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
evalStatement env (Uncall (Identifier i) args)        = evalFunctionCall env (invert i) args
evalStatement env (If ifExp tb eb fiExp)              = evalIf env ifExp tb eb fiExp
evalStatement env (LoopUntil from d l until)          = evalWhile env from until d l 
evalStatement env (Log   lhss)                        = evalLog env lhss
evalStatement env (LocalVarDeclaration var i stmts e) = evalLocalVarDec env var i stmts e

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
        where lhsToString (LHSIdentifier (Identifier name)) = name

-- Evaluates a "#log" statement.
evalLog :: Env -> [LHS] -> Q EvalState
evalLog env xs = do
    throwLogExceptionIfNecessary
    -- We take the tail because the first Exp is a separator.
    let logUpdateStmts = ListE $ tail $ evalLogUpdate xs
    tmpN <- newName "tmp''"
    let concatedList = (AppE (toE "concat") logUpdateStmts)
    let zero = LitE $ IntegerL 0
    let traceExp  = AppE (AppE (toE "trace") concatedList) zero
    return ([letStmt (BangP $ VarP tmpN) traceExp], [], env)

-- Throws an exception if the user has "#log" statements in their code, but
-- has not imported Debug.Trace.
throwLogExceptionIfNecessary :: Q ()
throwLogExceptionIfNecessary = do
    ModuleInfo mods <- thisModule >>= reifyModule
    let canLog = any sDebugMod mods
    if not canLog then
        error "You need to add the Haskell line 'import Debug.Trace' at the top (outside the Oxford brackets) of the file in which you use the Hanus #log statement."
    else
        return ()
        where sDebugMod (Module _ (ModName "Debug.Trace")) = True
              sDebugMod _                                  = False

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
    x <- argsE lhss
    let call = letStmt valuesP (AppE (AppE vf x) exp)
    restores <- concatMapM restoreStmt (zip vNames lhss)
    return (call:restores, [], env)
    where argsE lst = do 
            x <- mapM argE lst
            return $ TupE x
          argE lhs = case lhs of
                       (LHSIdentifier ident) -> return $ VarE $ nameId ident
                       (LHSArray lhs exp)    -> do
                         x <- argE lhs
                         return $ AppE (AppE ((VarE . mkName) "indexerGet") x) exp
                       (LHSField obj field)  -> do 
                         get <- [|(\(FieldIndexer g _) -> g)|]
                         let fi = AppE get (VarE $ nameId field)
                         x <- argE obj;
                         return $ AppE fi x 
          restoreStmt (vname, lhs) = 
            case lhs of 
              (LHSIdentifier ident) -> return $ [letStmt (VarP $ nameId ident) (VarE vname)]
              (LHSArray lhs exp)    -> do
                tmpN <- newName "tmp''"
                x <- argE lhs
                let res = letStmt (VarP tmpN) (AppE (AppE (AppE ((VarE . mkName) "indexerSet") x) exp) (VarE vname))
                return [res, letStmt (lhsP lhs) (VarE tmpN)]
              (LHSField obj field) -> do
                set <- [|(\(FieldIndexer _ s) -> s)|]
                tmpN <- newName "tmp''"
                x <- argE obj
                let res = letStmt (VarP tmpN) (AppE (AppE (AppE set (VarE $ nameId field)) x) (VarE vname))
                return [res, letStmt (lhsP lhs) (VarE tmpN)]
          lhsP lhs = case lhs of
                       (LHSIdentifier ident) -> VarP $ nameId ident
                       (LHSField obj _)  -> lhsP obj


-- Evaluate a janus procedure call to it's corresponding TH representation
evalFunctionCall :: Env -> String -> [LHS] -> Q EvalState
evalFunctionCall env@(TupP globalsList, _) name args = do
    tmpN <- newName "tmp''"
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
    tmpN <- newName "tmp''"
    f <- foldM (\exp pat -> do
                    arg <- expFromVarP pat
                    return (AppE exp arg))
         (VarE name)
         args
    return ([letStmt (VarP tmpN) f, letStmt (fst env) (VarE tmpN)], [], env)

-- Evaluates a list of Statements, given a certain Env, and returns the
-- evaluated list of Statements in a do block, together with possible
-- nested declarations and an updated environment.
evalBranch :: [Statement] -> Env -> Q (Exp, [Dec], Env)
evalBranch b env = do
    stmts <- foldM accResult (initR env) b
    let e = DoE ((frst stmts) ++ [(NoBindS (tupP2tupE (snd env)))])
    return (e, scnd stmts, thrd stmts)

-- Convert a sequence of statements in an EvalState to a do block.
branchToDoExp :: EvalState -> Env -> Q Exp
branchToDoExp stmts env
    = return $ DoE ((frst stmts) ++ [(NoBindS (tupP2tupE (snd env)))])

-- Evaluate a janus 'if' statement to it's corresponding TH representation
evalIf :: Env -> Exp -> [Statement] -> [Statement] -> Exp -> Q EvalState
evalIf env ifExp tb eb fiExp = do
    (b1,decls1,_) <- evalBranch tb env
    (b2,decls2,_) <- evalBranch eb env
    tmpN <- newName "tmp''"
    gN1  <- newName "guardRes1'"
    gN2  <- newName "guardRes2'"
    let ifGuardStmt             = letStmt (VarP gN1) ifExp
    let fiGuardStmt             = letStmt (VarP gN2) fiExp
    let ifExp                   = CondE (VarE gN1) b1 b2
    let ifStmt                  = letStmt (VarP tmpN) ifExp
    let updateStmt              = letStmt (snd env) (VarE tmpN)
    let assertExp               = AppE (AppE (VarE $ mkName "==") (VarE gN1)) (VarE gN2)
    assertFailExp              <- runQ [|error "The Boolean result of if guard == Boolean result of fi guard."|]
    (assertStmts,decls,newEnv) <- evalIfErr env assertExp ([], [], env) [NoBindS assertFailExp]
    let stmts = ifGuardStmt : ifStmt : updateStmt : fiGuardStmt : assertStmts
    return (stmts, decls ++ decls1 ++ decls2, newEnv)

evalIfErr :: Env -> Exp -> EvalState -> [Stmt] -> Q EvalState
evalIfErr env g tb eb = do
    b1     <- branchToDoExp tb env
    let b2  = DoE eb
    tmpN   <- newName "tmp''"
    let ifExp  = CondE g b1 b2
    let ifStmt = letStmt (VarP tmpN) ifExp
    return ([ifStmt, letStmt (snd env) (VarE tmpN)], [], env)

evalSingleBranchIf :: Env -> Exp -> [Stmt] -> Q EvalState
evalSingleBranchIf env g eb = do
    b1 <- branchToDoExp ([],[],env) env
    b2 <- branchToDoExp (eb,[],env) env
    tmpN <- newName "tmp''"
    let ifExp  = CondE g b1 b2
    let ifStmt = letStmt (VarP tmpN) ifExp
    return ([ifStmt, letStmt (snd env) (VarE tmpN)], [], env)

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
    whileProcName <- newName "loop''"
    whileProcCall <- evalFunctionCallWithName (scope, scope) whileProcName scope
    -- The while loop can only be evaluated if fromGuard is true the first time (and *only* the first time).
    err           <- runQ [|error "From-guard in while loop was not true upon first evaluation."|]
    -- The err will be thrown in a do block that should return a value, so we actually
    -- have to return a bogus value after we throw the error in order to please Haskell.
    let errStmt    = NoBindS $ AppE err (tupP2tupE (snd env))
    whileIf       <- evalIfErr env fromGuard whileProcCall [errStmt]

    err           <- runQ [|error "From-guard in while loop was true at some point *after* the first iteration."|]
    -- The err will be thrown in a do block that should return a value, so we actually
    -- have to return a bogus value after we throw the error in order to please Haskell.
    let errStmt             = NoBindS $ AppE err (tupP2tupE (snd env))
    whileProcLoopIf        <- evalIfErr env (AppE (VarE (mkName "not")) fromGuard) whileProcCall [errStmt]
    (loopStmts, loopDecls) <- evalStmts loopStatements
    let whileProcLoopBlock  = loopStmts ++ (frst whileProcLoopIf)

    whileProcDoIf          <- evalSingleBranchIf env untilGuard whileProcLoopBlock
    (doStmts, doDecls)     <- evalStmts doStatements
    let whileProcBlock      = doStmts ++ (frst whileProcDoIf)

    whileProcDec           <- actualEvalProcedure' whileProcName scope whileProcBlock -- the empty list here shouldn't be empty.

    return (frst whileIf, [whileProcDec] ++ loopDecls ++ doDecls, env)

    where evalStmts stmts = do
              stmts <- foldM accResult (initR env) stmts
              return $ (frst stmts, scnd stmts)

evalLocalVarDec :: Env -> Variable -> Exp -> [Statement] -> Exp -> Q EvalState
evalLocalVarDec env v@(Variable (Identifier varName) _) init body exit = do
    varPat <- varToPat v
    tmpN <- newName "tmp''"
    let env' = (fst env, (TupP . (:) varPat. unwrapTupleP . snd) env)
    stmts <- foldM accResult (initR env') body
    doReturn <- localVarReturnStmt env varName exit
    let body' = DoE ((frst stmts) ++ [doReturn])
    let asg = LetE [ValD varPat (NormalB init) []] body'
    return ([letStmt (VarP tmpN) asg, letStmt (snd env) (VarE tmpN)], scnd stmts, env)

-- An if-then-else statement that returns the tuple with updated values, *IF*
-- the local var has the value that the programmer stated at delocal. If they
-- didn't, an error will be thrown.
localVarReturnStmt :: Env -> String -> Exp -> Q Stmt
localVarReturnStmt env varName exitCondition = do
    e1     <- [|"Variable '"|]
    let e2  = LitE $ StringL varName
    e3     <- [|"' had the value '"|]
    let e4  = AppE (toE "show") (toE varName)
    e5     <- [|"' when it was delocalized, but it should have had the value '"|]
    let e6  = AppE (toE "show") exitCondition
    e7     <- [|"'."|]

    let errMsg = AppE (toE "concat") (ListE [e1,e2,e3,e4,e5,e6,e7])
    let guard  = AppE (AppE (toE "==") (toE varName)) exitCondition
    let tb     = DoE [NoBindS (tupP2tupE $ snd env)]
    let eb     = AppE (toE "error") errMsg

    return $ NoBindS $ CondE guard tb eb


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

-- Accumulator used for folding all statements in a procedure
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

-- The inverse of a procedure call proc_call_name is stored as proc_call_name'
invert :: String -> String
invert name = name ++ "'"
