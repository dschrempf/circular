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
import Data.Stack.Circular
import Data.Vector (Vector)
import qualified Data.Vector as V
import Test.Hspec
import Test.Hspec.QuickCheck
-- import Prelude hiding (replicate)
import Test.QuickCheck.Instances.Vector ()

se :: CStack Int
se = empty 10

ss :: CStack Int
ss = push 13 se

prop_from_to_id :: Eq a => Vector a -> Bool
prop_from_to_id v
  | V.length v == 0 = True
  | otherwise = toVector (fromVector v) == v

prop_pop :: Eq a => Vector a -> Bool
prop_pop v
  | V.length v == 0 = True
  | otherwise = toVector (snd $ pop $ fromVector v) == V.init v

prop_push_pop :: Eq a => a -> Vector a -> Bool
prop_push_pop x v
  | V.length v == 0 = True
  | otherwise = toVector (snd $ pop $ push x $ fromVector v) == V.tail v

prop_push :: Eq a => a -> Vector a -> Bool
prop_push x v
  | V.length v == 0 = True
  | otherwise = toVector (push x $ fromVector v) == V.tail v V.++ V.singleton x

prop_many_pushes :: Eq a => [a] -> Vector a -> Bool
prop_many_pushes xs v
  | V.length v == 0 = True
  | length xs <= V.length v = True
  | otherwise =
    toVector (foldr push (fromVector v) xs)
      == V.fromList (reverse $ take (V.length v) xs)

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

  describe "pop" $
    prop "works on test cases" (prop_pop :: Vector Int -> Bool)

  describe "push" $
    prop "works on test cases" (prop_push :: Int -> Vector Int -> Bool)

  describe "push pop" $
    prop "works on test cases" (prop_push_pop :: Int -> Vector Int -> Bool)

  describe "many pushes" $
    prop "works on test cases" (prop_many_pushes :: [Int] -> Vector Int -> Bool)

  describe "laziness" $
    it "should not conflict with intuition" $
    toVector ss `shouldNotBe` toVector (push 10 ss)
