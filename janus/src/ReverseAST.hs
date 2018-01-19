module ReverseAST where

import AST

type Reversal a = a -> a

reverseProg :: Reversal Program
reverseProg (Program decls) =
  Program $ map reverseDecl decls

reverseDecl :: Reversal Declaration
reverseDecl (Procedure identifier vars block) =
  Procedure identifier vars (reverseBlock block)
reverseDecl glob = glob

reverseBlock :: Reversal Block
reverseBlock = reverse . map reverseStmt

reverseStmt :: Reversal Statement
reverseStmt stmt =
  case stmt of
    (Assignment rev op lhs e) -> Assignment (not rev) op lhs e
    (Call identifier lhs) -> Uncall identifier lhs
    (Uncall identifier lhs) -> Call identifier lhs
    (If e s s' e') -> If e' (reverseBlock s) (reverseBlock s') e
    (LoopUntil e s s' e') -> LoopUntil e' (reverseBlock s) (reverseBlock s') e
    (LocalVarDeclaration var e s e') -> LocalVarDeclaration var e' (reverseBlock s) e
    l@(Log _) -> l
