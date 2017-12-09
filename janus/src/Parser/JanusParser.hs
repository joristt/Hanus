{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Parser.JanusParser where

import Parser.HaskellParser

import AST

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances
import Language.Haskell.TH.Syntax

parseFile :: FilePath -> IO Program
parseFile file = do
                    content <- readFile file
                    putStrLn $ "Source " ++ content
                    let program = parser content
                    putStrLn $ "Program " ++ show program
                    return $ program

parser :: String -> Program
parser s = parse (pProgram <* pEnd) (createStr (LineColPos 0 0 0) s)

parser1 x s = parse (x <* pEnd) (createStr (LineColPos 0 0 0) s)

pKey keyw = pToken keyw `micro` 1 <* spaces
spaces :: Parser String
spaces = pMunch (`elem` " \n")

pGreedyChoice (x:xs) = x <<|> (pGreedyChoice xs)
pGreedyChoice [] = pFail

pProgram :: Parser Program
pProgram = Program <$ pSpaces <*> pList pDeclaration


pDeclaration :: Parser Declaration
pDeclaration = (pProcedure <<|> pGlobalVariableDeclaration ) <* pSpaces

pGlobalVariableDeclaration :: Parser Declaration
pGlobalVariableDeclaration = GlobalVarDeclaration <$> pVariable [";"] <* pSpaces

pVariable :: [String] -> Parser Variable
pVariable final = Variable <$> pIdentifier <* pSpaces <* pKey "::" <*> (fst <$> pType final)  

pIdentifier :: Parser Identifier
pIdentifier = Identifier <$> pMunch (\t -> 'a' <= t && t <= 'z') <* pSpaces

pProcedure :: Parser Declaration
pProcedure = Procedure <$ pKey "procedure" <* pSpaces <*> pIdentifier <* pKey "(" <*> pVariableList <*> pBlock
            where pVariableList = (:) <$> pVariable [","] <*> pVariableList <<|> (:[]) <$> pVariable [")"]

pBlock :: Parser Block
pBlock = pList (pStatement <* pSpaces)

pStatement :: Parser Statement
pStatement = pGreedyChoice [
                    pAssignement,
                    pCall,
                    pUncall,
                    pIf,
                    pLoop,
                    pLocalVariable
                ]




pAssignement :: Parser Statement
pAssignement = Assignement <$> pOperator <*> (pList pLHS) <*> (fst <$> pExp [";"])

pOperator :: Parser String
pOperator = ((++) <$> pGreedyChoice [
                pKey "+",
                pKey "-",
                pKey "^"
            ]) <*> pKey "=" <* pSpaces

pLHS :: Parser LHS
pLHS = pGreedyChoice [
            pLHSIdentifier,
            pLHSArray,
            pLHSField
        ]

pLHSIdentifier :: Parser LHS
pLHSIdentifier = LHSIdentifier <$> pIdentifier <* pSpaces

pLHSArray :: Parser LHS
pLHSArray = LHSArray <$> pLHS <* (pKey "[") <*> (fst <$> pExp ["]"]) <* pSpaces

pLHSField :: Parser LHS
pLHSField = LHSField <$> pLHS <* (pKey ".") <*> pIdentifier <* pSpaces

pCall :: Parser Statement
pCall = Call <$ pKey "call" <*> pIdentifier <*> pList pLHS

pUncall :: Parser Statement
pUncall = Uncall <$ pKey "uncall" <*> pIdentifier <*> pList pLHS


pLocalVariable :: Parser Statement 
pLocalVariable = LocalVarDeclaration <$ pKey "local" <*> 
                    pVariable ["="] <*> (fst <$> pExp [";"]) <*> 
                    pBlock <*
                    pKey "delocal" <*> (fst <$> pExp [";"])

pIf :: Parser Statement
pIf = If <$ pToken "if" <* pSpaces
  <*> (fst <$> pExp ["then"]) <* pSpaces <*> pBlock <* pSpaces
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

