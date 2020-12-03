{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
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
import Control.Monad
import Control.Monad.Primitive
import Control.Monad.ST
import Data.Aeson
import qualified Data.Stack.Circular as C
import qualified Data.Vector as VB
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck hiding (Result, Success)
import Test.QuickCheck.Instances.Vector ()
import Prelude hiding (product, sum)

instance Arbitrary (C.Stack VB.Vector Int) where
  arbitrary = do
    s <- getSize
    n <- choose (1, s + 1)
    v <- VB.fromList <$> vector n
    i <- choose (0, n -1)
    return $ C.Stack v i

se :: PrimMonad m => m (C.MStack VB.Vector (PrimState m) Int)
se = C.replicate 10 0

ss :: PrimMonad m => m (C.MStack VB.Vector (PrimState m) Int)
ss = se >>= C.push 13

fromTo :: VB.Vector Int -> VB.Vector Int
fromTo v = C.toVector $ runST $ C.fromVector v >>= C.freeze

prop_from_to_id :: VB.Vector Int -> Bool
prop_from_to_id v
  | VB.length v == 0 = True
  | otherwise = fromTo v == v

prop_push_get :: Int -> VB.Vector Int -> Bool
prop_push_get x v
  | VB.length v == 0 = True
  | otherwise = x == runST (C.fromVector v >>= C.push x >>= C.get)

prop_push :: Int -> VB.Vector Int -> Bool
prop_push x v
  | VB.length v == 0 = True
  | otherwise =
    C.toVector (runST $ C.fromVector v >>= C.push x >>= C.freeze)
      == VB.tail v VB.++ VB.singleton x

prop_many_pushes :: [Int] -> VB.Vector Int -> Bool
prop_many_pushes xs v
  | VB.length v == 0 = True
  | length xs <= VB.length v = True
  | otherwise =
    C.toVector
      ( runST $ do
          ms <- C.fromVector v
          ms' <- foldM (flip C.push) ms xs
          C.freeze ms'
      )
      == sol
  where
    nl = length xs
    nv = VB.length v
    -- That was hard :).
    sol = VB.drop nl v VB.++ VB.fromList (reverse $ take nv $ reverse xs)

prop_json :: C.Stack VB.Vector Int -> Bool
prop_json c = Success c == fromJSON (toJSON c)

spec :: Spec
spec = do
  describe "construction" $
    it "doesn't choke on weird inputs" $ do
      C.toVector (runST $ se >>= C.freeze) `shouldBe` VB.replicate 10 0
      runST (ss >>= C.get) `shouldBe` 13

  describe "conversion identities" $ do
    it "correctly converts partly filled stacks" $
      C.toVector (runST $ ss >>= C.freeze) `shouldBe` (VB.replicate 9 0 `VB.snoc` 13)
    prop "toVector . fromVector is identity" (prop_from_to_id :: VB.Vector Int -> Bool)

  describe "conversion failure" $
    it "fails to convert empty vectors" $
      evaluate (runST $ C.fromVector VB.empty >>= C.freeze) `shouldThrow` anyErrorCall

  describe "properties" $ do
    prop "push" prop_push
    prop "push_get" prop_push_get
    prop "many pushes" prop_many_pushes
    prop "json" prop_json
