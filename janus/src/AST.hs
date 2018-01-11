module AST where

import Language.Haskell.TH.Syntax
import Data.List

newtype Program = Program [Declaration]

data Declaration = GlobalVarDeclaration Variable Exp
                 | Procedure Identifier [Variable] Block

data Variable = Variable Identifier Type

newtype Identifier = Identifier String deriving (Show, Eq, Ord)

data LHS = LHSIdentifier Identifier
         | LHSArray LHS Exp
         | LHSField LHS Identifier

type Block = [Statement]

data Statement
  -- x y s= exp
  -- pop x y
  = Assignment String [LHS] (Maybe Exp)
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
  show (Program decls) = intercalate "\n" $ map show decls

instance Show Declaration where
  show (GlobalVarDeclaration var) = "Decl: " ++ show var
  show (Procedure identifier variables blocks) = "procedure " ++ show identifier ++ " (" ++ intercalate "," (map show variables) ++ ")\n" ++ intercalate "\n" (map show blocks)

instance Show Variable where
  show (Variable identifier t) = show identifier ++ "::" ++ show t

instance Show Identifier where
  show (Identifier s) = s

instance Show LHS where
  show (LHSIdentifier identifier) = show identifier
  show (LHSArray lhs exp) = show lhs ++ "[" ++ show exp ++ show "]"
  show (LHSField lhs identifier) = show lhs ++ "." ++ show identifier

instance Show Statement where
  show (Assignment operator lhs exp ) = intercalate ", " (map show lhs) ++ operator ++ show exp
  show (Call identifier lhs) = "call " ++ show identifier ++ " " ++ unwords (map show lhs)
  show (Uncall identifier lhs) = "uncall " ++ show identifier ++ " " ++ unwords (map show lhs)
