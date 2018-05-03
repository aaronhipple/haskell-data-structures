module Data.Tree.BinarySpec (main, spec) where

import Test.Hspec
import Test.QuickCheck

import Data.Tree.Binary

-- `main` is here so that this module can be run from GHCi on its own.  It is
-- not needed for automatic spec discovery.
main :: IO ()
main = hspec spec

spec :: Spec
spec = do
  describe "BinaryTree" $ do
    it "constructs successfully" $ do
      Leaf 0 Empty Empty `shouldBe` Leaf 0 Empty Empty
  describe "insert" $ do
    it "inserts an item into an empty tree" $ do
      insert 5 Empty `shouldBe` Leaf 5 Empty Empty
    it "inserts a lesser item to the left" $ do
      insert 3 (Leaf 5 Empty Empty) `shouldBe` Leaf 5 (Leaf 3 Empty Empty) Empty
    it "inserts a greater item to the right" $ do
      insert 7 (Leaf 5 Empty Empty) `shouldBe` Leaf 5 Empty (Leaf 7 Empty Empty)
    it "recursively inserts an item" $ do
      insert 4 (Leaf 5 (Leaf 3 Empty Empty) Empty) `shouldBe` Leaf 5 (Leaf 3 Empty (Leaf 4 Empty Empty)) Empty
  describe "contains" $ do
    it "determines if a tree contains a value" $ do
     contains 5 (Leaf 5 (Leaf 3 Empty (Leaf 4 Empty Empty)) Empty) `shouldBe` True
     contains 9 (Leaf 5 (Leaf 3 Empty (Leaf 4 Empty Empty)) Empty) `shouldBe` False
  describe "inOrder" $ do
    it "sorts the list" $ do 
      inOrder (Leaf 5 (Leaf 3 Empty (Leaf 4 Empty Empty)) Empty)
      `shouldBe` [3,4,5] 
