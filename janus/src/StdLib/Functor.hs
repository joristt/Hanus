module StdLib.Functor where

import Data.Functor
import StdLib.Operator
import StdLib.DefaultValue

-- Example:
-- x :: Int[];
-- x =$ complement;
(=$), (🏦) :: Functor f => Operator (f a) (Operator a ())
(=$) = Operator g h
  where
    g f (Operator fwd _) = fmap (`fwd` ()) f
    h f (Operator _ bwd) = fmap (`bwd` ()) f

(🏦) = (=$)

-- Example:
-- x :: Int[];
-- x =$$ (+=, 1);
-- (The inverse of this statement is `x =$$ (-=, 1);`)
(=$$), (🏦🏦) :: Functor f => Operator (f a) (Operator a b, b)
(=$$) = Operator g h
  where
    g f (Operator fwd _, arg) = fmap (`fwd` arg) f
    h f (Operator _ bwd, arg) = fmap (`bwd` arg) f

(🏦🏦) = (=$$)
