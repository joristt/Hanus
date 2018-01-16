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
push = Operator (\(stack, val) _ -> (val : stack, defaultValue)) stack_push 
pop  = inverseOf push stack_push

stack_push :: (Eq a, Eq b, DefaultValue a, DefaultValue b) => ([b], a) -> p -> ([b], b)
stack_push (s:stack, value) _
  | value == defaultValue = (stack, s)
  | otherwise = error "pop: Second argument is not the default value"
stack_push ([], _) _ = error "pop: Stack is empty"
