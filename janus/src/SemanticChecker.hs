module SemanticChecker (extractVarsE, semanticCheck) where

import           AST
import           Data.List                  (nub, (\\))
import           Language.Haskell.TH.Syntax

semanticCheck :: Program -> Bool
semanticCheck prog = and [check prog | check <- [ mainExists
                                                , rhsCheck
                                                ]]

-- | Check that main exists.
mainExists :: Program -> Bool
mainExists (Program decls) =
  "main" `elem` foldl1 (++) (map getProcName decls)
  where getProcName :: Declaration -> [String]
        getProcName (Procedure (Identifier name) _ _) = [name]
        getProcName _                                 = []

-- | Check that variables on the LHS does not appear on the RHS.
rhsCheck :: Program -> Bool
rhsCheck (Program decls) = all rhsCheckD decls

rhsCheckD :: Declaration -> Bool
rhsCheckD (Procedure _ _ stmts) = rhsCheckB stmts
rhsCheckD _                     = True

rhsCheckB :: Block -> Bool
rhsCheckB = all rhsCheckS

rhsCheckS :: Statement -> Bool
rhsCheckS stmt = case stmt of
  (Assignement _ lh e) ->
    all (`notElem` eVars) (map lhsToId lh)
    where eVars = extractVarsE e
          lhsToId :: LHS -> String
          lhsToId (LHSIdentifier (Identifier name)) = name
          lhsToId (LHSArray lhs _)                  = lhsToId lhs
          lhsToId (LHSField lhs _)                  = lhsToId lhs
  (If _ b b' _) -> all rhsCheckB [b, b']
  (LoopUntil _ b b' _) -> all rhsCheckB [b, b']
  (LocalVarDeclaration _ _ b _) -> rhsCheckB b
  _ -> True

-- | Variable extraction.
extractVarsE :: Exp -> [String]
extractVarsE = nub . extractVarsE'
extractVarsE' :: Exp -> [String]
extractVarsE' e = case e of
  (VarE name) -> [nameBase name]
  (AppE e e') -> concatVarsE [e, e']
  (InfixE lhs op rhs) ->
    case (lhs, rhs) of
      (Just lhs, Just rhs) -> concatVarsE [lhs, op, rhs]
      (Nothing, Just rhs)  -> concatVarsE [op, rhs]
      (Just lhs, Nothing)  -> concatVarsE [lhs, op]
  (UInfixE e e' e'') -> concatVarsE [e, e', e'']
  (ParensE e) -> extractVarsE' e
  (LamE pats e) ->
    extractVarsE' e \\ foldl1 (++) (map extractVarsP pats)
  (LamCaseE _) ->
    error "GHC extension 'LambdaCase' not supported."
  (TupE es) -> concatVarsE es
  (UnboxedTupE es) -> concatVarsE es
  (CondE g e e') -> concatVarsE [g, e, e']
  (MultiIfE _) ->
    error "GHC extension 'MultiWayIf' not supported."
  (LetE _ _) ->
    error "Let expressions not supported."
  (CaseE _ _) ->
    error "Case expressions not supported."
  (DoE _) ->
    error "Do expressions not supported."
  (CompE _) ->
    error "Comprehension expressions not supported."
  (ArithSeqE range) ->
    concatVarsE $ case range of
      FromR e              -> [e]
      FromThenR e e'       -> [e, e']
      FromToR e e'         -> [e, e']
      FromThenToR e e' e'' -> [e, e', e'']
  (ListE es) -> concatVarsE es
  (SigE e _) -> extractVarsE' e
  (RecConE _ fieldExps) ->
    concatVarsE $ map snd fieldExps
  (RecUpdE e fieldExps) ->
    concatVarsE $ e : map snd fieldExps
  (StaticE e) -> extractVarsE' e
  (UnboundVarE name) -> [nameBase name]
  _ -> []

extractVarsP :: Pat -> [String]
extractVarsP p = case p of
  (VarP name)        -> [nameBase name]
  (TupP pats)        -> concatVarsP pats
  (UnboxedTupP pats) -> concatVarsP pats
  (ParensP pat)      -> extractVarsP pat
  (TildeP pat)       -> extractVarsP pat
  (BangP pat)        -> extractVarsP pat
  (AsP name pat)     -> nameBase name : extractVarsP pat
  (ListP pats)       -> concatVarsP pats
  (SigP pat _)       -> extractVarsP pat
  (ViewP _ pat)      -> extractVarsP pat
  _                  -> []

concatVars :: (a -> [String]) -> [a] -> [String]
concatVars extF es = foldl1 (++) $ map extF es
concatVarsE :: [Exp] -> [String]
concatVarsE = concatVars extractVarsE'
concatVarsP :: [Pat] -> [String]
concatVarsP = concatVars extractVarsP
