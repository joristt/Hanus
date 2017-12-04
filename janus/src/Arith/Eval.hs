{-# LANGUAGE QuasiQuotes #-}

module Arith.Eval where

import Arith.AST
import Arith.QQ

-- | Evaluator.
eval :: Expr -> Integer
eval [arith| `e` + `e'` |] = eval e + eval e'
eval (Lit i) = i
