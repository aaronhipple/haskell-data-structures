module Data.Tree.Binary (BinaryTree(..))  where

data BinaryTree a
  = Empty
  | Leaf a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)
