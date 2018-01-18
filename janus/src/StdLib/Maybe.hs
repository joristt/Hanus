module StdLib.Maybe where

import StdLib.DefaultValue
import StdLib.Operator

instance DefaultValue (Maybe a) where
  defaultValue = Nothing

createMaybe :: (Eq a, DefaultValue a) => Operator (Maybe a) ()
createMaybe = inverseOf destructMaybe f
  where
    f Nothing _ = Just defaultValue
    f _ _ = error "createMaybe: Expected Nothing, got Just instead"

destructMaybe ::(Eq a, DefaultValue a) => Operator (Maybe a) ()
destructMaybe = inverseOf createMaybe f
  where
    f (Just x) _
      | x == defaultValue = Nothing
      | otherwise = error "destructMaybe: Expected a Just with defaultValue, got a different value"
    f _ _ = error "createMaybe: Expected Just, got Nothing instead"
