module Data.Tree.Binary
  ( BinaryTree(..)
  , insert
  , contains
  , inOrder
  , sort
  ) where

data BinaryTree a
  = Empty
  | Leaf a
         (BinaryTree a)
         (BinaryTree a)
  deriving (Eq, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert newValue Empty = Leaf newValue Empty Empty
insert newValue (Leaf currentValue left right)
  | currentValue > newValue = (Leaf currentValue (insert newValue left) right)
  | otherwise = (Leaf currentValue left (insert newValue right))

contains :: Ord a => a -> BinaryTree a -> Bool
contains _ Empty = False
contains value (Leaf checkValue left right)
  | checkValue == value = True
  | checkValue < value = contains value right
  | checkValue > value = contains value left

inOrder :: Ord a => BinaryTree a -> [a]
inOrder Empty = []
inOrder (Leaf value left right) = concat [inOrder left, [value], inOrder right]

fromList :: Ord a => [a] -> BinaryTree a
fromList = foldr insert Empty

sort :: Ord a => [a] -> [a]
sort xs = inOrder $ fromList xs
