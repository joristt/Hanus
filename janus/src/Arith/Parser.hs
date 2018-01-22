{-# LANGUAGE FlexibleContexts #-}

module Arith.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char)
import Text.Parsec.Expr (buildExpressionParser, Operator (Infix), Assoc (AssocLeft))
import Text.Parsec.Language (haskell)
import qualified Text.Parsec.Token as Tokens

import Language.Haskell.TH (Exp)
import Language.Haskell.Meta (parseExp)

import Arith.AST

-- | Run given parser with location information.
-- (needed for displaying errors inside quasi-quotations)
runParserWithLoc :: Monad m => (String, Int, Int) -> String -> m Prog
runParserWithLoc (file, line, col) s =
    case runParser p () "" s of
      Left err -> fail $ show err
      Right e -> return e
  where
    p = do pos <- getPosition
           setPosition $
            flip setSourceName file $
            flip setSourceLine line $
            setSourceColumn pos col
           name <- nameP <~ ":"
           e <- exprP <* eof
           return $ Prog name e

-- | Parser for arithmetic expressions.
exprP :: Parser Expr
exprP = buildExpressionParser [[infix_ "+" Add]] expr
  where expr = try (parens exprP) <|>
               try metaExpP <|>
               Lit <$> numP

-- | Primitive parsers.
nameP :: Parser String
nameP = Tokens.identifier haskell
numP :: Parser Integer
numP = lexeme $ Tokens.integer haskell
metaExpP :: Parser Expr
metaExpP = do
  s <- "`" ~> many1 (noneOf ['`']) <~ "`"
  case parseExp s of
    Left err -> fail $ "Not a valid Haskell expression: " ++ show err
    Right e -> return $ MetaExp e

-- | Useful macros.
lexeme p = spaces *> p <* spaces
(<~) p s = p <* lexeme (string s)
(~>) s p = lexeme (string s) *> p
parens p = lexeme $ Tokens.parens haskell p
infix_ op f = Infix (Tokens.reservedOp haskell op >> return f) AssocLeft
