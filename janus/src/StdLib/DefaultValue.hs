{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}

module StdLib.DefaultValue where

class DefaultValue a where
  defaultValue :: a

instance (Num a) => DefaultValue a where
  defaultValue = 0

instance DefaultValue Bool where
  defaultValue = False

instance DefaultValue [a] where
  defaultValue = []
