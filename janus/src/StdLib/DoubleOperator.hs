{-# LANGUAGE TemplateHaskell #-}
module StdLib.DoubleOperator where

import StdLib.DefaultValue
import Data.Bits


data DoubleOperator a b = DoubleOperator (a -> b -> (a, b)) (a -> b -> (a, b))

doubleInverseOf :: DoubleOperator a b -> (a -> b -> (a, b)) -> DoubleOperator a b
doubleInverseOf (DoubleOperator f _) g = DoubleOperator g f

(<=>) :: DoubleOperator a a
(<=>) = doubleInverseOf (<=>) (\x y -> (y, x))

push, pop :: (Eq a, DefaultValue a) => DoubleOperator [a] a
push = doubleInverseOf pop  (\stack value -> (value : stack, defaultValue))
pop  = doubleInverseOf push f
  where
    f (s:stack) value
      | value == defaultValue = (stack, s)
      | otherwise = error "pop: Second argument is not the default value"
    f [] _ = error "pop: Stack is empty"
