module StdLib.Operator where

import StdLib.DefaultValue
import qualified Data.Bits as B
import qualified Data.Map.Strict as M

data Operator a b = Operator (a -> b -> a) (a -> b -> a)

inverseOf :: Operator a b -> (a -> b -> a) -> Operator a b
inverseOf (Operator f _) g = Operator g f

symmetric :: (a -> b -> a)-> Operator a b
symmetric f = Operator f f

(+=), (-=) :: (Num a) => Operator a a
(+=) = Operator  (+) (-)
(-=) = inverseOf (+=) (-)

(^=) :: (B.Bits a) => Operator a a
(^=) = inverseOf (^=) B.xor

-- Usage: complement x;
complement :: (B.Bits a) => Operator a ()
complement = inverseOf complement (\x () -> B.complement x)

-- Usage: swap x y;
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

mapAdd :: (Ord k, Eq v, DefaultValue v) => Operator (M.Map k v) k
mapAdd = inverseOf mapRemove f
  where
    f m key = case M.lookup key m of
      Nothing -> M.insert key defaultValue m
      _ -> error "mapAdd: key already exists in this Map"

mapRemove :: (Ord k, Eq v, DefaultValue v) => Operator (M.Map k v) k
mapRemove = inverseOf mapAdd f
  where
    f m key = case M.lookup key m of
      Nothing -> error "mapRemove: key does not exist in this Map"
      (Just x)
        | x == defaultValue -> M.delete key m
        | otherwise -> error "mapRemove: value in Map is not equal to defaultValue"
