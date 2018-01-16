{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Parser.JanusParser where

import Parser.HaskellParser

import AST

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Text.ParserCombinators.UU.Utils
import Language.Haskell.TH.Syntax

keywords =
  [
    "procedure",
    "call",
    "uncall",
    "from",
    "do",
    "until",
    "loop",
    "if",
    "then",
    "else",
    "fi",
    "local",
    "delocal"
  ]

parser :: String -> Int -> Int -> String -> Program
parser = parser1 pProgram

parser1 :: Parser a -> String -> Int -> Int -> String -> a
parser1 p fileName line col s = parse_h (assertNoError <$> p <*> pEnd) $ createStr (LineCol line col) s
  where
    assertNoError :: a -> [Error LineCol] -> a
    assertNoError a []     = a
    assertNoError _ (e:es) = error $ "Parsing of Janus code failed in file " ++ fileName ++ ". First error:\n" ++ show e

parser2 :: Parser a -> String -> a
parser2 p = parser1 p "input" 0 0

foo = "if True then x += 1; else swap x y; fi 2 * 3 == 6;"
bar = "if n==10 then n+= 10; fi n == 20;"

isIf (If _ _ _ _) = True
isIf _ = False

pSomeSpace :: Parser String
pSomeSpace = (:) <$> pSatisfy (`elem` " \r\n\t") (Insertion "Whitespace" ' ' 1) <*> pSpaces

pKey :: String -> Parser String
pKey keyw = pToken keyw `micro` 1 <* pSpaces

pProgram :: Parser Program
pProgram = Program <$ pSpaces <*> pList pDeclaration

pDeclaration :: Parser Declaration
pDeclaration = (pProcedure <<|> pGlobalVariableDeclaration ) <* pSpaces

pGlobalVariableDeclaration :: Parser Declaration
pGlobalVariableDeclaration = GlobalVarDeclaration <$> (fst <$> pVariable [";"]) <* pSpaces

pVariable :: [String] -> Parser (Variable, String)
pVariable final = (\name (t, sep) -> (Variable name t, sep)) <$> pIdentifier <* pSpaces <* pKey "::" <*> pType final

pNonEmptyArgumentList :: Parser [Variable]
pNonEmptyArgumentList = addLength 10 (do
    (var, sep) <- pVariable [")", ","]
    pSpaces
    case sep of
      ")" -> return [var]
      "," -> (var :) <$> pNonEmptyArgumentList `micro` 1
  )

pName :: Parser String
pName = addLength 1 (do
    name <- (:) <$> pSatisfy validChar (Insertion "identifier" 'a' 1) <*> pMunch validChar
    if name `elem` keywords then do
        pSatisfy (== 'a') (Insertion "identifier" 'a' 1)
        return $ name ++ "a"
    else
        return name
  )
  where
    validChar t = 'a' <= t && t <= 'z'

pIdentifier :: Parser Identifier
pIdentifier = Identifier <$> pName

pProcedure :: Parser Declaration
pProcedure = Procedure
  <$  pToken "procedure"
  <*  pSomeSpace
  <*> pIdentifier
  <*  pSpaces
  <*  pKey "("
  <*> pVariableList
  <*  pSpaces
  <*  pKey "{"
  <*> pBlock
  <*  pSpaces
  <*  pKey "}"
            where pVariableList = ([] <$ pToken ")") <<|> pNonEmptyArgumentList

pBlock :: Parser Block
pBlock = pList_ng pStatement

pStatement :: Parser Statement
pStatement
  =   pAssignment
  <|> pCall
  <|> pPrefixOperatorAssignment
  <|> pIf
  <|> pLoop
  <|> pLocalVariable

pAssignment :: Parser Statement
pAssignment = (\x y z -> Assignment False y x z) <$> pSomeLHS <* pSpaces <*> pOperator <* pSpaces <*> (fst <$> pExp [";"]) <* pSpaces

pOperator :: Parser String
pOperator =
    (:) <$> pSatisfy (`elem` firstChar) (Insertion "Operator" (head firstChar) 1) <*> pMunch (`elem` validChars)
    <|> pSym '`' *> pName <* pSym '`'
  where
    -- ':' cannot be the first char as it can only be used for constructor operators
    validChars = ':' : firstChar
    firstChar = "!#$%&*+./<=>?@\\^|-~"

pSomeLHS :: Parser [LHS]
pSomeLHS = (:) <$> pLHS <*> pList_ng (pSomeSpace *> pLHS)

pLHS :: Parser LHS
pLHS = f <$> pIdentifier <*> pList_ng pLHSPart
  where
    f :: Identifier -> [Either Identifier Exp] -> LHS
    f name = foldl add (LHSIdentifier name)
    add :: LHS -> Either Identifier Exp -> LHS
    add prev (Left identifier) = LHSField prev identifier
    add prev (Right indexer)   = LHSArray prev indexer
    pLHSPart :: Parser (Either Identifier Exp)
    pLHSPart
      =   Left <$ pSpaces <* pKey "." <*> pIdentifier
      <|> Right <$ pSpaces <* pKey "[" <*> (fst <$> pExp ["]"])

pLHSIdentifier :: Parser LHS
pLHSIdentifier = LHSIdentifier <$> pIdentifier

pPrefixOperatorAssignment :: Parser Statement
pPrefixOperatorAssignment = Assignment False <$> pName <*> pList_ng (pSomeSpace *> pLHS) `micro` 1 <* pSpaces <* pToken ";" <*> pReturn (ConE (mkName "()")) <* pSpaces

pCall :: Parser Statement
pCall = ((Call <$ pToken "call") <|> (Uncall <$ pToken "uncall")) <* pSomeSpace <*> pIdentifier <*> pList_ng (pSomeSpace *> pLHS) <* pSpaces <* pToken ";" <* pSpaces

pLocalVariable :: Parser Statement
pLocalVariable = LocalVarDeclaration <$ pKey "local" <*>
                    (fst <$> pVariable ["="]) <*> (fst <$> pExp [";"]) <*>
                    pBlock <*
                    pKey "delocal" <*> (fst <$> pExp [";"])

pIf :: Parser Statement
pIf = If <$ pToken "if" <* pSomeSpace
  <*> (fst <$> pExp ["then"]) <* pSomeSpace <*> pBlock <* pSpaces
  <*> pElse <* pToken "fi" <* pSpaces <*> (fst <$> pExp [";"])
  <* pSpaces
  where
    pElse =
        -- With 'else'
        (pToken "else" *> pSomeSpace *> pBlock) <* pSpaces 
        <|>
        -- Without 'else'
        return []

pLoop :: Parser Statement
pLoop = addLength 10 (do
    pToken "loop"
    pSpaces
    (exp, sep) <- pExp ["do", "loop", "until"]
    pSpaces
    (doBlock, loopBlock, untilExp) <- (case sep of
        "do" -> pLoopAfterDo
        "loop" -> (\(b, e) -> ([], b, e)) <$> pLoopAfterLoop
        "until" -> (\e -> ([], [], e)) <$> pLoopAfterUntil) :: Parser (Block, Block, Exp)
    return $ LoopUntil exp doBlock loopBlock untilExp
    ) <* pSpaces

pLoopAfterDo :: Parser (Block, Block, Exp)
pLoopAfterDo = (\b1 (b2, e) -> (b1, b2, e)) <$ pSpaces <*> pBlock <* pSpaces <*> (
    (pToken "loop" *> pLoopAfterLoop)
    <<|> ((\e -> ([], e)) <$ pToken "until" <* pSpaces <*> pLoopAfterUntil)
  )

pLoopAfterLoop :: Parser (Block, Exp)
pLoopAfterLoop = (\b e -> (b, e)) <$> pBlock <* pSpaces <* pToken "until" <* pSpaces <*> pLoopAfterUntil

pLoopAfterUntil :: Parser Exp
pLoopAfterUntil = fst <$> pExp [";"] <* pSpaces
