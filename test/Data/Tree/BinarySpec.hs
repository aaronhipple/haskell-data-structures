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

