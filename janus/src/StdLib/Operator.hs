module StdLib.Operator where

import StdLib.DefaultValue
import Data.Bits

data Operator a b = Operator (a -> b -> a) (a -> b -> a)

inverseOf :: Operator a b -> (a -> b -> a) -> Operator a b
inverseOf (Operator f _) g = Operator g f

symmetric :: (a -> b -> a)-> Operator a b
symmetric f = Operator f f

(+=), (-=) :: (Num a) => Operator a a
(+=) = Operator  (+) (-)
(-=) = inverseOf (+=) (-)

(^=) :: (Bits a) => Operator a a
(^=) = inverseOf (^=) xor

swap :: Operator (a, a) ()
swap = symmetric (\(x, y) () -> (y, x))

push, pop :: (DefaultValue a, Eq a) => Operator ([a], a) ()
push = inverseOf pop (\(stack, val) _ -> (val : stack, defaultValue))
pop  = inverseOf push f
  where
    f (s:stack, value) _
      | value == defaultValue = (stack, s)
      | otherwise = error "pop: Second argument is not the default value"
    f ([], _) _ = error "pop: Stack is empty"
