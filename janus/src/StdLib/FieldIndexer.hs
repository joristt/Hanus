module StdLib.FieldIndexer where

data FieldIndexer a b = FieldIndexer (a -> b) (a -> b -> a) 

first :: FieldIndexer (a, b) a
first = FieldIndexer fst (\(_, b) a -> (a, b))

second :: FieldIndexer (a, b) b
second = FieldIndexer snd (\(a, _) b -> (a, b))
