module SemanticChecker (extractVarsE, semanticCheck) where

import           AST
import           Data.List                  (nub, (\\))
import           Language.Haskell.TH.Syntax


semanticCheck :: Program -> Maybe String
semanticCheck prog
  | not (mainExists prog) = Just "main procedure not found"
  | not (rhsCheck prog) = Just "assigned variable appears on the left-hand side"
  | otherwise = Nothing

-- | Check that main exists.
mainExists :: Program -> Bool
mainExists (Program decls) =
  any checkMain decls
  where
    checkMain :: Declaration -> Bool
    checkMain (Procedure (Identifier "main") _ _) = True
    checkMain _                                   = False

-- | Check that variables on the LHS does not appear on the RHS.
rhsCheck :: Program -> Bool
rhsCheck (Program decls) =
  all rhsCheckD decls
  where
    rhsCheckD :: Declaration -> Bool
    rhsCheckD (Procedure _ _ stmts) = rhsCheckB stmts
    rhsCheckD _                     = True
    rhsCheckB :: Block -> Bool
    rhsCheckB = all rhsCheckS
    rhsCheckS :: Statement -> Bool
    rhsCheckS stmt = case stmt of
      (If _ b b' _) ->
        all rhsCheckB [b, b']
      (LoopUntil _ b b' _) ->
        all rhsCheckB [b, b']
      (LocalVarDeclaration _ _ b _) ->
        rhsCheckB b
      (Assignment _ _ lhs e) ->
        all (`notElem` usedVars) names
        where
          (names, es) = fmap concat $ unzip $ map lhsInfo lhs
          usedVars = concatMap extractVarsE (e:es)
          lhsInfo :: LHS -> (String, [Exp])
          lhsInfo lhs = case lhs of
            (LHSIdentifier (Identifier name)) ->
              (name, [])
            (LHSArray lhs e) ->
              let (name, es) = lhsInfo lhs
              in (name, e:es)
            (LHSField lhs (Identifier name)) ->
              let (_, es) = lhsInfo lhs
              in (name, es)
      _ -> True

-- | Variable extraction from Haskell expressions.
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

-- | Variable extraction from Haskell patterns.
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
