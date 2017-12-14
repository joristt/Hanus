{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Parser.JanusParser where

import Parser.HaskellParser

import AST

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
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

parseFile :: FilePath -> IO Program
parseFile file = do
                    content <- readFile file
                    putStrLn $ "Source " ++ content
                    let program = parser content
                    putStrLn $ "Program " ++ show program
                    return $ program

parser :: String -> Program
parser = parser1 pProgram

parser1 :: Parser a -> String -> a
parser1 p s = case execParser p s of
    (a, [])   -> a
    (_, e:es) -> error $ "Parsing failed. First error:\n" ++ show e

pSomeSpace :: Parser String
pSomeSpace = (:) <$> pSatisfy (`elem` " \r\n\t") (Insertion "Whitespace" ' ' 1) <*> pSpaces

pKey :: String -> Parser String
pKey keyw = pToken keyw `micro` 1 <* pSpaces

pGreedyChoice (x:xs) = x <<|> (pGreedyChoice xs)
pGreedyChoice [] = pFail

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
        return $ name
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
pBlock = pList pStatement

pStatement :: Parser Statement
pStatement
  =   pAssignement
  <|> pCall
  <|> pPrefixOperatorAssignment
  <|> pIf
  <|> pLoop
  <|> pLocalVariable
    {- pGreedyChoice [
                    -- pAssignement,
                    pCall,
                    pIf,
                    pLoop,
                    pLocalVariable
                ] -}

pAssignement :: Parser Statement
pAssignement = (\x y z -> Assignement y x (Just z)) <$> pSomeLHS <* pSpaces <*> pOperator <* pSpaces <*> (fst <$> pExp [";"]) <* pSpaces

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
pLHS = pGreedyChoice [
            pLHSIdentifier {-,
            pLHSArray,
            pLHSField -}
        ]

pLHSIdentifier :: Parser LHS
pLHSIdentifier = LHSIdentifier <$> pIdentifier

pLHSArray :: Parser LHS
pLHSArray = LHSArray <$> pLHS <* pSpaces <* pKey "[" <*> (fst <$> pExp ["]"])

pLHSField :: Parser LHS
pLHSField = LHSField <$> pLHS <* (pKey ".") <*> pIdentifier

pPrefixOperatorAssignment :: Parser Statement
pPrefixOperatorAssignment = Assignement <$> pName <*> pList_ng (pSomeSpace *> pLHS) <* pSpaces <* pToken ";" <*> pReturn Nothing

pCall :: Parser Statement
pCall = ((Call <$ pToken "call") <|> (Uncall <$ pToken "uncall")) <* pSomeSpace <*> pIdentifier <*> pList_ng (pSomeSpace *> pLHS) <* pSpaces <* pToken ";"

pLocalVariable :: Parser Statement 
pLocalVariable = LocalVarDeclaration <$ pKey "local" <*> 
                    (fst <$> pVariable ["="]) <*> (fst <$> pExp [";"]) <*> 
                    pBlock <*
                    pKey "delocal" <*> (fst <$> pExp [";"])

pIf :: Parser Statement
pIf = If <$ pToken "if" <* pSomeSpace
  <*> (fst <$> pExp ["then"]) <* pSomeSpace <*> pBlock <* pSpaces
  <*> pElse <* pSpaces <* pToken "fi" <* pSpaces <*> (fst <$> pExp [";"])
  where
    pElse = 
        -- With 'else'
        (pToken "else" *> pBlock)
        <<|>
        -- Without 'else'
        (return ([]))

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
    )

pLoopAfterDo :: Parser (Block, Block, Exp)
pLoopAfterDo = (\b1 (b2, e) -> (b1, b2, e)) <$ pSpaces <*> pBlock <* pSpaces <*> (
    (pToken "loop" *> pLoopAfterLoop)
    <<|> ((\e -> ([], e)) <$ pToken "until" <* pSpaces <*> pLoopAfterUntil)
  )

pLoopAfterLoop :: Parser (Block, Exp)
pLoopAfterLoop = (\b e -> (b, e)) <$> pBlock <* pSpaces <* pToken "until" <* pSpaces <*> pLoopAfterUntil

pLoopAfterUntil :: Parser Exp
pLoopAfterUntil = fst <$> pExp [";"] <* pSpaces

