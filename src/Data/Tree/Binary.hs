module Data.Tree.Binary (BinaryTree(..), insert, contains, inOrder)  where

data BinaryTree a
  = Empty
  | Leaf a (BinaryTree a) (BinaryTree a)
  deriving (Eq, Show)

insert :: Ord a => a -> BinaryTree a -> BinaryTree a
insert newValue Empty = Leaf newValue Empty Empty
insert newValue (Leaf currentValue left right)
  | currentValue > newValue =
    case left of
      Empty -> Leaf currentValue (Leaf newValue Empty Empty) right
      _ -> Leaf currentValue (insert newValue left) right
  | otherwise =
    case right of
      Empty -> Leaf currentValue left (Leaf newValue Empty Empty)
      _ -> Leaf currentValue left (insert newValue left)

contains :: Ord a => a  -> BinaryTree a -> Bool
contains _ Empty = False
contains value (Leaf checkValue left right) 
  | checkValue == value = True
  | checkValue < value  = contains value right
  | checkValue > value  = contains value left



inOrder :: Ord a => BinaryTree a -> [a] 
inOrder Empty = []
inOrder (Leaf value left right) = concat [inOrder left, [value], inOrder right]