{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- |
-- Module      :  Data.Stack.Circular
-- Description :  Circular stacks of fixed size
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Thu Jun 18 10:00:28 2020.
module Data.Stack.Circular
  ( -- * Boxed circular stacks
    CStack (stack, index, curSize),

    -- * Construction
    empty,

    -- * Conversion
    toVector,
    toVectorN,
    fromVector,

    -- * Accessors
    get,
    pop,
    push,
    unsafePush,

    -- * Queries
    isFull,

    -- * Folding
    --
    -- Here all fold functions should be provided, but I am too lazy. Instead,
    -- let's just provide some optimized functions to compute summary statistics
    -- across all values on the stack.
    --
    -- For reasons of efficiency, __commutativity__ of the combining function is
    -- __assumed__ for fold-like functions provided in this section! That is,
    -- the order of elements of the stack must not matter.
    foldl1',
    sum,
    mean,
    product,
  )
where

import Control.Monad.ST
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Vector.Generic as V
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic.Mutable as M
import Prelude hiding (product, sum)

-- | Circular stacks with fxed maximum size are just normal vectors with a
-- pointer to the last element.
--
-- Construction of 'CStack's is done with 'empty' and subsequent 'push'es, or
-- the provided type conversion functions so that the index and bounds are
-- updated and checked consistently.
--
-- When denoting the efficiency of the functions @m@ refers to the current size
-- of the stack, and @n@ to the maximum size.
data CStack v a = CStack
  { stack :: v a,
    index :: !Int,
    curSize :: !Int
  }

instance (Eq (v a), Vector v a) => Eq (CStack v a) where
  (CStack v1 i1 m1) == (CStack v2 i2 m2) = (v1 == v2) && (i1 == i2) && (m1 == m2)

instance (Show (v a), Vector v a) => Show (CStack v a) where
  show c@(CStack _ i m) = "CStack {" ++ show (toVector c) ++ ", " ++ show i ++ ", " ++ show m ++ "}"

-- | We have @c /= FromJSON $ ToJSON c@, but the elements on the stack and their
-- order are correctly saved and restored.
instance (ToJSON a, ToJSON (v a), Vector v a) => ToJSON (CStack v a) where
  toJSON c = object ["stack" .= toVector c, "maxSize" .= n]
    where
      n = V.length $ stack c
  toEncoding c = pairs ("stack" .= toVector c <> "maxSize" .= n)
    where
      n = V.length $ stack c

instance (FromJSON a, FromJSON (v a), Vector v a) => FromJSON (CStack v a) where
  parseJSON = withObject "CStack" fromObject

fromObject :: forall v a. (FromJSON (v a), Vector v a) => Object -> Parser (CStack v a)
fromObject o = do
  v <- o .: "stack" :: Parser (v a)
  n <- o .: "maxSize" :: Parser Int
  let c = empty n
  pure $ V.foldr' unsafePush c v

-- Calculate the start index of the stack.
--
-- (startIndex + m - 1) `mod` n = i
startIndex :: Int -> Int -> Int -> Int
startIndex i m n
  | m == 0 = error "startIndex: empty stack"
  | m <= i + 1 = i + 1 - m
  | otherwise = i + 1 - m + n

-- | A circular stack without an element but of a given maximum size. At this
-- state, it is not very useful :). O(n).
empty :: Vector v a => Int -> CStack v a
empty n
  | n <= 0 = error "empty: maximum size must be 1 or larger"
  | otherwise = CStack (V.create $ M.unsafeNew n) 0 0

-- | Convert a circular stack to a vector. The first element of the returned
-- vector is the deepest (oldest) element of the stack, the last element of the
-- returned vector is the current (newest) element of the stack.
--
-- This is a relatively expensive operation. O(m).
toVector :: Vector v a => CStack v a -> v a
toVector (CStack v i m)
  | m == 0 = V.empty
  | i' + m <= n = V.unsafeSlice i' m v
  | otherwise = V.unsafeDrop i' v V.++ V.unsafeTake (i + 1) v
  where
    n = V.length v
    i' = startIndex i m n

-- | Convert the last N elements of a circular stack to a vector. The first
-- element of the returned vector is the deepest (oldest) element of the stack,
-- the last element of the returned vector is the current (newest) element of
-- the stack.
--
-- The size of the stack must be larger than N.
--
-- This is a relatively expensive operation. O(N).
toVectorN :: Vector v a => Int -> CStack v a -> v a
toVectorN k (CStack v i m)
  | k < 0 = error "toVectorN: negative n"
  | k > m = error "toVectorN: stack too small"
  | k == 0 = V.empty
  | i' + k <= n = V.unsafeSlice i' k v
  | otherwise = V.unsafeDrop i' v V.++ V.unsafeTake (i + 1) v
  where
    n = V.length v
    i' = startIndex i k n

-- | Convert a vector to a circular stack. The first element of the vector is
-- the deepest (oldest) element of the stack, the last element of the vector is
-- the current (newest) element of the stack. O(n).
--
-- The vector must be non-empty.
fromVector :: Vector v a => v a -> CStack v a
fromVector v
  | V.null v = error "fromVector: empty vector"
  | otherwise = CStack v (n - 1) n
  where
    n = V.length v

-- | Get the last element without changing the stack. O(1).
get :: Vector v a => CStack v a -> a
get (CStack v i _) = V.unsafeIndex v i

-- Select the previous element without changing the stack.
previous :: Vector v a => CStack v a -> CStack v a
previous (CStack v i m)
  | m == 0 = error "previous: empty stack"
  | i == 0 = CStack v (n - 1) (m - 1)
  | otherwise = CStack v (i - 1) (m - 1)
  where
    n = V.length v

-- | Get the last element and remove it from the stack. O(1).
--
-- The stack must be non-empty.
pop :: Vector v a => CStack v a -> (a, CStack v a)
pop c = (get c, previous c)

-- Replace an element in a vector.
set :: Vector v a => Int -> a -> v a -> v a
set i x = V.modify (\v -> M.write v i x)
{-# INLINE set #-}

-- Replace the last element.
put :: Vector v a => a -> CStack v a -> CStack v a
put x (CStack v i m) = CStack (set i x v) i m

-- Select the next element without changing the stack.
next :: Vector v a => CStack v a -> CStack v a
next (CStack v i m)
  | i == (n - 1) = CStack v 0 (min (m + 1) n)
  | otherwise = CStack v (i + 1) (min (m + 1) n)
  where
    n = V.length v

-- | Push an element on the stack. O(n).
push :: Vector v a => a -> CStack v a -> CStack v a
push x c = put x $ next c

unsafeSet :: Vector v a => Int -> a -> v a -> v a
unsafeSet i x v = runST $ do
  mv <- V.unsafeThaw v
  M.unsafeWrite mv i x
  V.unsafeFreeze mv

-- Replace the last element. O(1).
unsafePut :: Vector v a => a -> CStack v a -> CStack v a
unsafePut x (CStack v i m) = CStack (unsafeSet i x v) i m

-- | Push an element on the stack. O(1).
--
-- Be careful; the internal vector is mutated! The immutable circular stack may
-- not be used after this operation.
unsafePush :: Vector v a => a -> CStack v a -> CStack v a
unsafePush x c = unsafePut x $ next c

-- | Check if the stack is full.
isFull :: Vector v a => CStack v a -> Bool
isFull (CStack v _ m) = V.length v == m

-- | Compute summary statistics of the elements on the stack using a custom
-- commutative `mappend` function.
foldl1' :: Vector v a => (a -> a -> a) -> CStack v a -> a
foldl1' f (CStack v i m)
  | m == n = V.foldl1' f v
  | i' + m <= n = V.foldl1' f $ V.unsafeSlice i' m v
  | otherwise = f (V.foldl1' f (V.unsafeDrop i' v)) (V.foldl1' f (V.unsafeTake (i + 1) v))
  where
    n = V.length v
    i' = startIndex i m n

-- | Compute the sum of the elements on the stack.
sum :: (Num a, Vector v a) => CStack v a -> a
sum (CStack v i m)
  | m == n = V.sum v
  | i' + m <= n = V.sum $ V.unsafeSlice i' m v
  | otherwise = V.sum (V.unsafeDrop i' v) + V.sum (V.unsafeTake (i + 1) v)
  where
    n = V.length v
    i' = startIndex i m n

-- | Compute the mean of the elements on the stack.
mean :: (Real a, Vector v a, Fractional b) => CStack v a -> b
mean c = realToFrac (sum c) / fromIntegral (curSize c)

-- | Compute the product of the elements on the stack.
--
-- For reasons of efficiency, commutativity of the combining function is
-- assumed. That is, the order of elements of the stack must not matter.
product :: (Num a, Vector v a) => CStack v a -> a
product (CStack v i m)
  | m == n = V.product v
  | i' + m <= n = V.product $ V.unsafeSlice i' m v
  | otherwise = V.product (V.unsafeDrop i' v) * V.product (V.unsafeTake (i + 1) v)
  where
    n = V.length v
    i' = startIndex i m n
