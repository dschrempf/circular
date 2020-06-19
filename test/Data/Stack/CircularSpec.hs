{-# LANGUAGE FlexibleInstances #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

-- |
--   Module      :  Data.Stack.CircularSpec
--   Description :  Unit tests for Data.Stack.Circular
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
import Data.Aeson
import Data.Stack.Circular as C
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Success, Result)
import Test.QuickCheck.Instances.Vector ()
import Prelude hiding (sum, product)

instance Arbitrary (CStack Vector Int) where
  arbitrary = do
    s <- getSize
    n <- choose (1, s+1)
    v <- V.fromList <$> vector n
    i <- choose (0, n-1)
    m <- choose (1, n)
    return $ CStack v i m

se :: CStack Vector Int
se = empty 10

ss :: CStack Vector Int
ss = push 13 se

prop_from_to_id :: Vector Int -> Bool
prop_from_to_id v
  | V.length v == 0 = True
  | otherwise = toVector (fromVector v) == v

prop_pop :: Vector Int -> Bool
prop_pop v
  | V.length v == 0 = True
  | otherwise = toVector (snd $ pop $ fromVector v) == V.init v

prop_push_pop :: Int -> Vector Int -> Bool
prop_push_pop x v
  | V.length v == 0 = True
  | otherwise = toVector (snd $ pop $ push x $ fromVector v) == V.tail v

prop_push :: Int -> Vector Int -> Bool
prop_push x v
  | V.length v == 0 = True
  | otherwise = toVector (push x $ fromVector v) == V.tail v V.++ V.singleton x

prop_many_pushes :: [Int] -> Vector Int -> Bool
prop_many_pushes xs v
  | V.length v == 0 = True
  | length xs <= V.length v = True
  | otherwise =
    toVector (foldr push (fromVector v) xs)
      == V.fromList (reverse $ take (V.length v) xs)

prop_length :: CStack Vector Int -> Bool
prop_length c = V.length (toVector c) == curSize c

jsonId :: CStack Vector Int -> Result (CStack Vector Int)
jsonId c = fromJSON $ toJSON c

prop_json_sum :: CStack Vector Int -> Bool
prop_json_sum c = (sum <$> jsonId c) == Success (sum c)

prop_json_product :: CStack Vector Int -> Bool
prop_json_product c = (product <$> jsonId c) == Success (product c)

-- Check current size and max size.
prop_json_misc :: CStack Vector Int -> Bool
prop_json_misc c = ((curSize <$> jsonId c) == Success (curSize c)) &&
                   ((V.length . stack <$> jsonId c) == Success (V.length $ stack c))

spec :: Spec
spec = do
  describe "construction" $ it "doesn't choke on weird inputs" $ do
    print ss
    toVector se `shouldBe` V.empty
    toVector (snd $ pop ss) `shouldBe` V.empty

  describe "conversion identities" $ do
    it "correctly converts partly filled stacks" $
      toVector ss `shouldBe` V.singleton 13
    prop "toVector . fromVector is identity" (prop_from_to_id :: Vector Int -> Bool)

  describe "conversion failure" $
    it "fails to convert empty vectors" $
      evaluate (fromVector V.empty) `shouldThrow` anyErrorCall

  describe "properties" $ do
    prop "pop" prop_pop
    prop "push" prop_push
    prop "push pop" prop_push_pop
    prop "many pushed" prop_many_pushes
    prop "length" prop_length
    prop "json sum" prop_json_sum
    prop "json product" prop_json_product
    prop "json misc" prop_json_misc

  describe "laziness" $
    it "should not conflict with intuition" $
    toVector ss `shouldNotBe` toVector (push 10 ss)
