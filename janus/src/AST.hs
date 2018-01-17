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
  = Assignment Bool String [LHS] Exp
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
  -- #log expr1 expr2 ...;
  -- This prints the exprs in format "expr1name : expr1value, expr2name : expr2value" etc...
  | Log [LHS]

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
  show (Assignment toReverse operator lhs exp ) = intercalate ", " (map show lhs) ++ (if toReverse then "~" else "") ++ operator ++ show exp
  show (Call identifier lhs) = "call " ++ show identifier ++ " " ++ unwords (map show lhs)
  show (Uncall identifier lhs) = "uncall " ++ show identifier ++ " " ++ unwords (map show lhs)
  show (If pre s1 s2 post) = "if " ++ show pre ++ " then\n"
    ++ indent (unlines $ map show s1)
    ++ "else\n"
    ++ indent (unlines $ map show s2)
    ++ "fi " ++ show post ++ ";"
  show (LoopUntil pre s1 s2 post) = "from " ++ show pre ++ "\ndo\n"
    ++ indent (unlines $ map show s1)
    ++ "loop\n"
    ++ indent (unlines $ map show s2)
    ++ "until " ++ show post ++ ";"
  show (LocalVarDeclaration v init block exp) = "local " ++ show v ++ " = " ++ show init ++ ";\n"
    ++ indent (unlines $ map show block)
    ++ "delocal " ++ show exp ++ ";"

indent :: String -> String
indent = unlines . map ("  " ++) . lines
