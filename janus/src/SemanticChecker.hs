module SemanticChecker (extractVarsE, semanticCheck) where

import Data.List ((\\), nub)
import Language.Haskell.TH.Syntax
import AST

semanticCheck :: Program -> Bool
semanticCheck (Program decls) = all semanticCheckD decls

semanticCheckD :: Declaration -> Bool
semanticCheckD (Procedure _ _ stmts) = semanticCheckB stmts
semanticCheckD _ = True

semanticCheckB :: Block -> Bool
semanticCheckB = all semanticCheckS

semanticCheckS :: Statement -> Bool
semanticCheckS (Assignement _ lh e) =
  all (`notElem` eVars) (map lhsToId lh)
  where eVars = extractVarsE e
        lhsToId :: LHS -> String
        lhsToId (LHSIdentifier (Identifier name)) = name
        lhsToId (LHSArray lhs _) = lhsToId lhs
        lhsToId (LHSField lhs _) = lhsToId lhs
semanticCheckS (If _ b b' _) = all semanticCheckB [b, b']
semanticCheckS (LoopUntil _ b b' _) = all semanticCheckB [b, b']
semanticCheckS (LocalVarDeclaration _ _ b _) = semanticCheckB b
semanticCheckS _ = True

extractVarsE :: Exp -> [String]
extractVarsE = nub . extractVarsE'
extractVarsE' :: Exp -> [String]
extractVarsE' (VarE name) = [nameBase name]
extractVarsE' (AppE e e') = concatVarsE [e, e']
extractVarsE' (InfixE lhs op rhs) =
  case (lhs, rhs) of
    (Just lhs, Just rhs) -> concatVarsE [lhs, op, rhs]
    (Nothing, Just rhs) -> concatVarsE [op, rhs]
    (Just lhs, Nothing) -> concatVarsE [lhs, op]
    _ -> []
extractVarsE' (UInfixE e e' e'') = concatVarsE [e, e', e'']
extractVarsE' (ParensE e) = extractVarsE' e
extractVarsE' (LamE pats e) =
  extractVarsE' e \\ foldl1 (++) (map extractVarsP pats)
extractVarsE' (LamCaseE _) =
  error "GHC extension 'LambdaCase' not supported."
extractVarsE' (TupE es) = concatVarsE es
extractVarsE' (UnboxedTupE es) = concatVarsE es
extractVarsE' (CondE g e e') = concatVarsE [g, e, e']
extractVarsE' (MultiIfE _) =
  error "GHC extension 'MultiWayIf' not supported."
extractVarsE' (LetE _ _) =
  error "Let expressions not supported."
extractVarsE' (CaseE _ _) =
  error "Case expressions not supported."
extractVarsE' (DoE _) =
  error "Do expressions not supported."
extractVarsE' (CompE _) =
  error "Comprehension expressions not supported."
extractVarsE' (ArithSeqE range) =
  concatVarsE $ case range of
    FromR e -> [e]
    FromThenR e e' -> [e, e']
    FromToR e e' -> [e, e']
    FromThenToR e e' e'' -> [e, e', e'']
extractVarsE' (ListE es) = concatVarsE es
extractVarsE' (SigE e _) = extractVarsE' e
extractVarsE' (RecConE _ fieldExps) =
  concatVarsE $ map snd fieldExps
extractVarsE' (RecUpdE e fieldExps) =
  concatVarsE $ e : map snd fieldExps
extractVarsE' (StaticE e) = extractVarsE' e
extractVarsE' (UnboundVarE name) = [nameBase name]
extractVarsE' _ = []

extractVarsP :: Pat -> [String]
extractVarsP (VarP name) = [nameBase name]
extractVarsP (TupP pats) = concatVarsP pats
extractVarsP (UnboxedTupP pats) = concatVarsP pats
extractVarsP (ParensP pat) = extractVarsP pat
extractVarsP (TildeP pat) = extractVarsP pat
extractVarsP (BangP pat) = extractVarsP pat
extractVarsP (AsP name pat) = nameBase name : extractVarsP pat
extractVarsP (ListP pats) = concatVarsP pats
extractVarsP (SigP pat _) = extractVarsP pat
extractVarsP (ViewP _ pat) = extractVarsP pat
extractVarsP _ = []

concatVars :: (a -> [String]) -> [a] -> [String]
concatVars extF es = foldl1 (++) $ map extF es
concatVarsE :: [Exp] -> [String]
concatVarsE = concatVars extractVarsE'
concatVarsP :: [Pat] -> [String]
concatVarsP = concatVars extractVarsP
