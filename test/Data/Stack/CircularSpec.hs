-- |
--   Module      :  Data.Stack.CircularSpec
--   Description :  Unit tests for Data.Vector.Circular
--   Copyright   :  (c) Dominik Schrempf, 2020
--   License     :  GPL-3.0-or-later
--
--   Maintainer  :  dominik.schrempf@gmail.com
--   Stability   :  unstable
--   Portability :  portable
--
-- Creation date: Thu Jun 18 10:21:28 2020.
module Data.Stack.CircularSpec
  ( spec,
  )
where

import Control.Exception
import Data.Stack.Circular
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Prelude hiding (replicate)
import Test.QuickCheck.Instances.Vector ()

vs :: CVector Int
vs = singleton 1

vm :: CVector Int
vm = fromList [0 .. 100]

vl :: CVector Int
vl = fromList [0 .. 1000]

prop_from_to_id :: Eq a => Vector a -> Bool
prop_from_to_id v
  | V.length v == 0 = True
  | otherwise = toVector (fromVector v) == v

prop_from_to_id_list :: Eq a => [a] -> Bool
prop_from_to_id_list xs
  | null xs = True
  | otherwise = toList (fromList xs) == xs

spec :: Spec
spec = do
  describe "construction"
    $ it "doesn't choke on weird inputs"
    $ toVector vs `shouldBe` V.singleton 1

  describe "conversion identities" $
    do
      prop
        "fromVector . toVector is identity"
        (prop_from_to_id :: Vector Int -> Bool)
      prop "fromList . toList is identity" (prop_from_to_id_list :: [Int] -> Bool)

  describe "conversion" $
    do
      it "correctly wraps around bounds" $
        do
          toVectorN 20 vs `shouldBe` V.replicate 20 1
          V.length (toVectorN 109 vm) `shouldBe` 109
          toListN 109 vm `shouldBe` ([0 .. 100] ++ [0 .. 7])
          length (toListN 109 vm) `shouldBe` 109
      it "fails to convert too short lists" $
        evaluate (fromListN 100 []) `shouldThrow` anyErrorCall

  describe "laziness"
    $ it "should not conflict with intuition"
    $ vl `shouldNotBe` put 10 vl

  describe "shift"
    $ it "should work as expected for a few sample cases"
    $ do
      toListN 1001 (shift 100 vl) `shouldBe` [100 .. 1000] ++ [0 .. 99]
      toListN 8129 (shift 100 vl) `shouldBe` toListN 8129 (shift 1101 vl)
