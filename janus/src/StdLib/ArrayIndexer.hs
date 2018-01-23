{-# LANGUAGE MultiParamTypeClasses, InstanceSigs #-}
{-# LANGUAGE FlexibleInstances, FunctionalDependencies #-}

module StdLib.ArrayIndexer where

import StdLib.DefaultValue

import Data.Maybe
import qualified Data.Map.Strict as M

type Array k v = M.Map k v

class ArrayIndexer a b c | a -> b c where
  indexerGet :: a -> b -> c
  indexerSet :: a -> b -> c -> a

instance Ord k => ArrayIndexer (M.Map k v) k v where
  indexerGet map key = fromMaybe (error "Map indexer: key not found." :: v) $ M.lookup key map
  indexerSet map key value = M.insert key value map
