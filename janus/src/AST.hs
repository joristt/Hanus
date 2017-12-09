module AST where

import Language.Haskell.TH.Syntax
import Data.List

newtype Program = Program [Declaration]

data Declaration = GlobalVarDeclaration Variable
                 | Procedure Identifier [Variable] Block

data Variable = Variable Identifier Type

newtype Identifier = Identifier String
              
data LHS = LHSIdentifier Identifier
         | LHSArray LHS Exp
         | LHSField LHS Identifier

type Block = [Statement]

data Statement
  -- x y s= exp
  -- pop x y
  = Assignement String [LHS] Exp 
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
  -- delocal expr;
  | LocalVarDeclaration Variable Exp Block Exp

{-
Examples:

x += expr
Assignment "+=" (LHSIdentifier "x") (expr)

swap x y 
(syntactic sugar for (x, y) `swap` ())
Assignment "swap" (LHSIdentifier "x", LHSIdentifier "y") ()
-}

instance Show Program where
  show (Program decls) = show (length decls) ++ (intercalate "\n" $ map show decls)

instance Show Declaration where
  show (GlobalVarDeclaration var) = show var
  --show (Procedure identifier variables blocks) = show identifier ++ " " ++ concatMap show variables ++ "\n" ++ concatMap show blocks

instance Show Variable where
  show (Variable identifier t) = show identifier ++ "::" ++ show t 

instance Show Identifier where
  show (Identifier s) = s