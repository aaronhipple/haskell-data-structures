module Data.Tree.Binary (BinaryTree(..), insert, contains)  where

data BinaryTree a
  = Empty
  | Leaf a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert newValue leaf =
  case leaf of
    Empty -> Leaf newValue Empty Empty
    Leaf currentValue left right ->
      if currentValue > newValue then
        case left of
          Empty -> Leaf currentValue (Leaf newValue Empty Empty) right
          _ -> Leaf currentValue (insert newValue left) right
      else
        case right of
          Empty -> Leaf currentValue left (Leaf newValue Empty Empty)
          _ -> Leaf currentValue left (insert newValue left)

contains :: Ord a => a -> BinaryTree a -> Bool
contains value tree = False
