{-# LANGUAGE TemplateHaskell #-}
module AST where

import Language.Haskell.TH.Syntax

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
  = Assignement String LHS Exp 
  | Swap LHS LHS
  | Call Identifier [LHS]
  | Uncall Identifier [LHS]
  | If Exp Block Block Exp
  | LoopUntil Exp Block Block Exp
  | LocalVarDeclaration Variable Exp Block Exp
