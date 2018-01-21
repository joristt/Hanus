{-# LANGUAGE MultiParamTypeClasses, InstanceSigs #-}
{-# LANGUAGE FlexibleInstances #-}

module StdLib.ArrayIndexer where

import StdLib.DefaultValue

import Data.Maybe
import qualified Data.Map.Strict as M

class (DefaultValue c) => ArrayIndexer a b c where
  indexerGet :: a -> b -> c
  indexerSet :: a -> b -> c -> a

instance (Ord k, DefaultValue v) => ArrayIndexer (M.Map k v) k v where
  indexerGet :: (M.Map k v) -> k -> v
  indexerGet map key = fromMaybe defaultValue $ M.lookup key map

  indexerSet :: (M.Map k v) -> k -> v -> M.Map k v
  indexerSet map key value = M.insert key value map