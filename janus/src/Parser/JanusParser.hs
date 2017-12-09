{-# LANGUAGE FlexibleContexts, Rank2Types #-}
module Parser.JanusParser where

import Parser.HaskellParser

import AST

import Text.ParserCombinators.UU
import Text.ParserCombinators.UU.Utils
import Text.ParserCombinators.UU.Core
import Text.ParserCombinators.UU.BasicInstances

parseFile :: FilePath -> IO Program
parseFile file = do
                    content <- readFile file
                    putStrLn $ "Source " ++ content
                    let program = parser content
                    putStrLn $ "Program " ++ show program
                    return $ program

parser :: String -> Program
parser s = parse pProgram (createStr (LineColPos 0 0 0) s)

parser1 x s = parse (addLength 0 x) (createStr (LineColPos 0 0 0) s)

pKey keyw = pToken keyw `micro` 1 <* spaces
spaces :: Parser String
spaces = pMunch (`elem` " \n")

pGreedyChoice (x:xs) = x <<|> (pGreedyChoice xs)
pGreedyChoice [] = pFail

pProgram = Program [] <$ pList (pSpaces *> pDeclaration <* pSpaces)

pDeclaration :: Parser Declaration
pDeclaration = (pGlobalVariableDeclaration <|> pProcedure) <* pSpaces

pGlobalVariableDeclaration :: Parser Declaration
pGlobalVariableDeclaration = GlobalVarDeclaration <$> pVariable <* pSpaces

pVariable :: Parser Variable
pVariable = Variable <$> pIdentifier <* pSpaces <* pKey "::" <*> (fst <$> pType [";"])  

pIdentifier :: Parser Identifier
pIdentifier = Identifier <$> pMunch (\t -> 'a' <= t && t <= 'z') <* pSpaces

pProcedure :: Parser Declaration
pProcedure = Procedure <$ pKey "procedure" <* pSpaces <*> pIdentifier <*> (pList pVariable) <*> pBlock

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

pIf= undefined
pLoop= undefined

pLocalVariable :: Parser Statement 
pLocalVariable = LocalVarDeclaration <$ pKey "local" <*> 
                    pVariable <* pKey "=" <*> (fst <$> pExp [";"]) <*> 
                    pBlock <*
                    pKey "delocal" <*> (fst <$> pExp [";"])