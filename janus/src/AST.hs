module AST where

import Language.Haskell.TH.Syntax

newtype Program = Program [Declaration]

data Declaration = GlobalVarDeclaration Variable Exp
                 | Procedure Identifier [Variable] Block

data Variable = Variable Identifier Type

newtype Identifier = Identifier String deriving (Eq, Ord)
              
--data LHS = LHSVariable Variable
data LHS = LHSIdentifier Identifier
         | LHSArray LHS Exp
         | LHSField LHS Identifier

type Block = [Statement]

data Statement
  -- x y s= exp
  -- pop x y
  = Assignement [LHS] Exp 
  -- call name x y
  | Call Identifier [LHS]
  -- uncall name x y
  | Uncall Identifier [LHS]
  -- if expr then
  --   block
  -- else
  --   block
  -- fi expr;
  | If Exp Block Block Exp
  -- from expr
  -- [ do block ]
  -- [ loop block ]
  -- until expr; 
  | LoopUntil Exp Block Block Exp
  -- local x :: T = expr;
  --   block
  -- delocal x = expr;
  | LocalVarDeclaration Variable Exp Block Exp

{-
Examples:

x += expr
Assignment "+=" (LHSIdentifier "x") (expr)

swap x y 
(syntactic sugar for (x, y) `swap` ())
Assignment "swap" (LHSIdentifier "x", LHSIdentifier "y") ()
-}