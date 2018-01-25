module StdLib.Operator where

import StdLib.DefaultValue
import qualified Data.Bits as B
import qualified Data.Map.Strict as M

data Operator a b = Operator (a -> b -> a) (a -> b -> a)

inverse :: Operator a b -> Operator a b
inverse (Operator f g) = Operator g f

symmetric :: (a -> b -> a)-> Operator a b
symmetric f = Operator f f

(+=), (-=) :: (Num a) => Operator a a
(+=) = Operator  (+) (-)
(-=) = inverse (+=)

(^=) :: (B.Bits a) => Operator a a
(^=) = symmetric B.xor

-- Usage: complement x;
complement :: (B.Bits a) => Operator a ()
complement = symmetric (\x () -> B.complement x)

-- Usage: swap x y;
swap :: Operator (a, a) ()
swap = symmetric (\(x, y) () -> (y, x))

push, pop :: (DefaultValue a, Eq a) => Operator ([a], a) ()

push = Operator (\(stack, val) _ -> (val : stack, defaultValue)) pop'
  where
    pop' (s:stack, value) _
      | value == defaultValue = (stack, s)
      | otherwise = error "pop: Second argument is not the default value"
    pop' ([], _) _ = error "pop: Stack is empty"

pop = inverse push

mapAdd :: (Ord k, Eq v, DefaultValue v) => Operator (M.Map k v) k
mapAdd = Operator add rm
  where
    add m key = case M.lookup key m of
      Nothing -> M.insert key defaultValue m
      _ -> error "mapAdd: key already exists in this Map"
    rm m key = case M.lookup key m of
      Nothing -> error "mapRemove: key does not exist in this Map"
      (Just x)
        | x == defaultValue -> M.delete key m
        | otherwise -> error "mapRemove: value in Map is not equal to defaultValue"
  

mapRemove :: (Ord k, Eq v, DefaultValue v) => Operator (M.Map k v) k
mapRemove = inverse mapAdd
