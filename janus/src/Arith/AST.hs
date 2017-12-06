{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}
{-# LANGUAGE TemplateHaskell #-}

module Arith.AST where

import Data.Generics
import Language.Haskell.TH (Q, Exp)
import Language.Haskell.TH.Syntax

-- | AST.
data Prog = Prog String Expr
data Expr = Lit Integer
          | Add Expr Expr
          | MetaExp Exp
          deriving (Eq, Show, Typeable, Data)

-- | Lifting.
class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr Exp where
  toExpr = MetaExp

instance ToExpr Integer where
  toExpr = Lit

-- | Code generation.
instance Lift Expr where
  lift (Lit i) = [| i |]
  lift (Add e e') = [| e + e' |]
  lift (MetaExp e) = return e

-- | Trivial semantic checker.
semanticChecker :: Expr -> Bool
semanticChecker (Add (Lit 0) _) = True
semanticChecker _ = False
