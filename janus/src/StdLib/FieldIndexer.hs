{-# LANGUAGE TemplateHaskell #-}
module StdLib.FieldIndexer where

data FieldIndexer a b = FieldIndexer (a -> b) (a -> b -> a) 

first :: FieldIndexer (a, b) a
first = FieldIndexer (\(a, _) -> a) (\(_, b) a -> (a, b))

second :: FieldIndexer (a, b) b
second = FieldIndexer (\(_, b) -> b) (\(a, _) b -> (a, b))
