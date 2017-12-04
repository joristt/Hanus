{-# LANGUAGE DeriveDataTypeable, FlexibleContexts, FlexibleInstances #-}

module Arith.AST where

import Data.Generics


-- | AST.
data Expr = Lit Integer
          | Add Expr Expr
          | MetaVar String
          deriving (Eq, Show, Typeable, Data)

-- | Lifting.
class ToExpr a where
  toExpr :: a -> Expr

instance ToExpr String where
  toExpr = MetaVar

instance ToExpr Integer where
  toExpr = Lit

-- | Trivial semantic checker.
semanticChecker :: Expr -> Bool
semanticChecker (Add (Lit 0) _) = True
semanticChecker _ = False
