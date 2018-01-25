module StdLib.BinaryTree where

import StdLib.DefaultValue
import StdLib.Operator
import StdLib.FieldIndexer

data BinaryTree a = Node a (BinaryTree a) (BinaryTree a) | Leaf deriving (Show, Eq, Ord)

instance DefaultValue (BinaryTree a) where
  defaultValue = Leaf

{-
Example:

a : BinaryTree Int;

createNode a;
a.nodeValue += 10;
createNode a.leftChild;
swap a.leftChild.nodeValue a.nodeValue;
swap a.leftChild b.rightChild;

-}

isNode, isLeaf :: BinaryTree a -> Bool
isNode Leaf = False
isNode _    = True
isLeaf = not . isNode

-- Indexers

nodeValue :: FieldIndexer (BinaryTree a) a
nodeValue = FieldIndexer get set
  where
    get Leaf = error ".nodeValue: got a leaf instead of a node"
    get (Node x _ _) = x
    set Leaf _ = error ".nodeValue: got a leaf instead of a node"
    set (Node _ l r) x = Node x l r

leftChild, rightChild :: FieldIndexer (BinaryTree a) (BinaryTree a)
leftChild = FieldIndexer get set
  where
    get Leaf = error ".leftChild: got a leaf instead of a node"
    get (Node _ l _) = l
    set Leaf _ = error ".leftChild: got a leaf instead of a node"
    set (Node x _ r) l = Node x l r
rightChild = FieldIndexer get set
  where
    get Leaf = error ".rightChild: got a leaf instead of a node"
    get (Node _ _ r) = r
    set Leaf _ = error ".rightChild: got a leaf instead of a node"
    set (Node x l _) r = Node x l r

createNode :: (DefaultValue a, Eq a) => Operator (BinaryTree a) ()
createNode = Operator create remove
  where
    create Leaf _ = Node defaultValue Leaf Leaf
    create _ _ = error "createNode: got a node instead of a leaf"
    remove Leaf _ = error "removeNode: got a leaf instead of a node"
    remove (Node x Leaf Leaf) _
      | x == defaultValue = Leaf
      | otherwise = error "removeNode: node had different value than defaultValue"
    remove (Node _ _ _) _ = error "removeNode: got a node with children, expected a node without children"

removeNode :: (DefaultValue a, Eq a) => Operator (BinaryTree a) ()
removeNode = inverse createNode
