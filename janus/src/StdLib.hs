module StdLib where

import Data.Bits


data Operator a b = Operator (a -> b -> a) (a -> b -> a)

data ArrayIndexer a b c = ArrayIndexer (a -> b -> Maybe c) (a -> b -> c -> a)

data FieldIndexer a b = FieldIndexer (a -> Maybe b) (a -> b -> a) 

class Value a where
    null :: a

inverseOf :: Operator a b -> (a -> b -> a) -> Operator a b
inverseOf (Operator f _) g = Operator g f

(+=), (-=) :: (Num a) => Operator a a
(+=) = inverseOf (-=) (+)
(-=) = inverseOf (+=) (-)

(^=) :: (Bits a) => Operator a a
(^=) = inverseOf (^=) (xor)

