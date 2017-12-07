{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}

module StdLib.ArrayIndexer where

import StdLib.DefaultValue

import Data.Maybe
import qualified Data.Map.Strict as M

class (DefaultValue c) => ArrayIndexer a b c where
  indexerGet :: a -> b -> c
  indexerSet :: a -> b -> c -> a

instance (DefaultValue v, Ord k) => ArrayIndexer (M.Map k v) k v where
  indexerGet map key = fromMaybe defaultValue $ M.lookup key map
  indexerSet map key value = M.insert key value map