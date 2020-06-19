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
import Data.Stack.Circular as C
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck
import Test.QuickCheck.Instances.Vector ()

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

spec :: Spec
spec = do
  describe "construction" $ it "doesn't choke on weird inputs" $ do
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

  describe "laziness" $
    it "should not conflict with intuition" $
    toVector ss `shouldNotBe` toVector (push 10 ss)
