{-# LANGUAGE FlexibleContexts #-}

module Arith.Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char)
import Text.Parsec.Expr (buildExpressionParser, Operator (Infix), Assoc (AssocLeft))
import Text.Parsec.Language (haskell)
import qualified Text.Parsec.Token as Tokens

import Arith.AST

-- | Run given parser with location information.
-- (needed for displaying errors inside quasi-quotations)
runParserWithLoc :: Monad m => (String, Int, Int) -> String -> m Expr
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
           exprP <* eof

-- | Parser for arithmetic expressions.
exprP :: Parser Expr
exprP = buildExpressionParser [[infix_ "+" Add]] expr
  where expr = try (parens exprP) <|>
               try ("`" ~> (MetaVar <$> metaVarP) <~ "`") <|>
               Lit <$> numP

-- | Primitive parsers.
numP :: Parser Integer
numP = lexeme $ Tokens.integer haskell
metaVarP :: Parser String
metaVarP = many1 (noneOf ['`'])

-- | Useful macros.
lexeme p = spaces *> p <* spaces
(<~) p s = p <* lexeme (string s)
(~>) s p = lexeme (string s) *> p
parens p = lexeme $ Tokens.parens haskell p
infix_ op f = Infix (Tokens.reservedOp haskell op >> return f) AssocLeft
