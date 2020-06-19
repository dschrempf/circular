{-# LANGUAGE RankNTypes #-}

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
    CStack (..),

    -- * Construction
    empty,

    -- * Conversion
    toVector,
    fromVector,

    -- * Accessors
    get,
    pop,
    push,
    unsafePush,
  )
where

import Control.Monad.ST
import qualified Data.Vector.Generic as V
import Data.Vector.Generic (Vector)
import qualified Data.Vector.Generic.Mutable as M

-- | Circular stacks with fxed maximum size are just normal vectors with a
-- pointer to the last element.
--
-- The type constructor 'CStack' is exported to create, for example, orphan
-- instances. However, construction of 'CStack's should happen with 'empty' and
-- subsequent 'push'es, 'replicate', or the provided type conversion functions
-- so that the index and bounds are updated and checked consistently.
--
-- When denoting the efficiency of the functions @m@ refers to the current size
-- of the stack, and @n@ to the maximum size.
data CStack v a = CStack
  { vector :: v a,
    index :: !Int,
    curSize :: !Int
  }

instance (Eq (v a), Vector v a) => Eq (CStack v a) where
  (CStack v1 i1 m1) == (CStack v2 i2 m2) = (v1 == v2) && (i1 == i2) && (m1 == m2)

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
    n  = V.length v
    i' = startIndex i m n

-- -- | Convert the last N elements of a circular stack to a vector. The first
-- -- element of the returned vector is the deepest (oldest) element of the stack,
-- -- the last element of the returned vector is the current (newest) element of
-- -- the stack. O(N).
-- --
-- -- The size of the stack must be larger than N.
-- toVectorN :: Int -> CStack a -> Vector a
-- toVectorN k (CStack v i m n)
--   | k < 0 = error "toVectorN: negative n"
--   | k > m = error "toVectorN: stack too small"
--   | k == 0 = V.empty
--   | i' + k <= n = V.unsafeSlice i' k v
--   | otherwise = V.unsafeDrop i' v V.++ V.unsafeTake (i + 1) v
--   where
--     i' = startIndex i k n

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
  where n = V.length v

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
  where n = V.length v

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

-- TODO.

-- | Here all fold functions should be provided, but I am too lazy. Instead,
-- let's just provide an optimized function to compute summary statistics across
-- all values on the stack.
--
-- For reasons of efficiency, commutativity of the combining function is
-- assumed. That is, the order of elements of the stack must not matter.
compute :: (a -> b) -> CStack v a -> b
compute = undefined

sum :: CStack v a -> a
sum = undefined

product :: CStack v a -> a
product = undefined
