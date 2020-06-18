{- |
   Module      :  Data.Vector.CircularSpec
   Description :  Unit tests for Data.Vector.Circular
   Copyright   :  (c) Dominik Schrempf, 2020
   License     :  GPL-3.0-or-later

   Maintainer  :  dominik.schrempf@gmail.com
   Stability   :  unstable
   Portability :  portable

Creation date: Thu Jun 18 10:21:28 2020.

-}

module Data.Vector.CircularSpec
  ( spec
  )
where

import           Control.Exception

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck.Instances.Vector ()

import           Data.Vector                      (Vector)
import qualified Data.Vector                      as V

import           Data.Vector.Circular

v1 :: CVector Int
v1 = empty

v2 :: CVector Int
v2 = singleton 1

prop_from_to_id :: Eq a => Vector a -> Bool
prop_from_to_id v = toVector (fromVector v) == v

prop_from_to_id_list :: Eq a => [a] -> Bool
prop_from_to_id_list v = toList (fromList v) == v

spec :: Spec
spec = do
  describe "creation" $
    it "doesn't choke on weird inputs" $ do
      toVector v1 `shouldBe` V.empty
      toVector v2 `shouldBe` V.singleton 1

  describe "conversion" $ do
    prop "fromVector . toVector is identity"
         (prop_from_to_id :: Vector Int -> Bool)
    prop "fromList . toList is identity" (prop_from_to_id_list :: [Int] -> Bool)

  describe "failed conversions" $
    it "sohuld fail to convert too short lists" $
      evaluate (fromListN 100 []) `shouldThrow` anyErrorCall

