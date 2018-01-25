module StdLib.Maybe where

import StdLib.DefaultValue
import StdLib.Operator

instance DefaultValue (Maybe a) where
  defaultValue = Nothing

createMaybe :: (Eq a, DefaultValue a) => Operator (Maybe a) ()
createMaybe = Operator create destruct
  where
    create Nothing _ = Just defaultValue
    create _ _ = error "createMaybe: Expected Nothing, got Just instead"
    destruct (Just x) _
      | x == defaultValue = Nothing
      | otherwise = error "destructMaybe: Expected a Just with defaultValue, got a different value"
    destruct _ _ = error "createMaybe: Expected Just, got Nothing instead"

destructMaybe ::(Eq a, DefaultValue a) => Operator (Maybe a) ()
destructMaybe = inverse createMaybe
