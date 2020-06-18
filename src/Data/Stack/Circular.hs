{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE DeriveGeneric #-}

-- |
-- Module      :  Data.Stack.Circular
-- Description :  Circular mutable vectors
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
    CVector (..),

    -- * Construction
    singleton,
    replicate,

    -- * Conversion
    toVector,
    toVectorN,
    fromVector,
    toList,
    toListN,
    fromList,
    fromListN,

    -- * Accessors
    get,
    getAt,
    pop,
    put,
    unsafePut,
    push,
    unsafePush,

    -- * Selectors
    next,
    previous,
    select,
    shift,
  )
where

import Control.Monad.ST
import qualified Data.Vector as V
import Data.Vector (Vector)
import GHC.Generics
import qualified Data.Vector.Mutable as M
import Prelude hiding (replicate)

-- | Circular stacks are just normal vectors with a pointer to a __selected
-- element__.
--
-- The type constructor 'CVector' is exported to create, for example, orphan
-- instances. However, construction of 'CVector's should happen with
-- 'singleton', 'replicate', or type conversion functions so that the bounds are
-- checked consistently.
--
-- When denoting the efficiency of the functions @m@ refers to the current size
-- of the stack, and @n@ to the maximum size.
data CVector a = CVector
  { vector :: Vector a,
    index :: !Int,
    curSize :: !Int,
    maxSize :: !Int
  }
  deriving (Show, Eq, Generic)

singletonV :: Int -> a -> Vector a
singletonV l x =
  V.create
    ( do
        v <- new l
        write v 1 x
        return v
    )

-- | A circular vector with one element and given maximum length. O(n).
singleton :: Int -> a -> CVector a
singleton l x = CVector (singletonV x l) 0 1 l

-- | Yield a filled circular vector with the same element. Select the first
-- element. O(n).
replicate :: Int -> a -> CVector a
replicate n x = CVector (V.replicate n x) 0 n

-- | Convert a circular vector to a classical vector. The first element of the
-- returned vector is the current __selected element__. O(m).
toVector :: CVector a -> Vector a
toVector (CVector v i m n) = V.unsafeSlice i (m - i) v V.++ V.unsafeTake i v

-- | Copy a circular vector to a classical vector of given length. Wrap around
-- the bound. The first element of the returned vector is the current selected
-- element. O(N).
toVectorN :: Int -> CVector a -> Vector a
toVectorN k (CVector v i m n) = V.generate k (\j -> v V.! ((i + j) `mod` m))

-- | Select the first element. O(1).
--
-- The given vector must be non-empty.
fromVector :: Vector a -> CVector a
fromVector v
  | V.null v = error "fromVector: empty vector"
  | otherwise = CVector v 0 (V.length v) (V.length v)

-- | Convert to a list with the selected element at the head. O(m).
toList :: CVector a -> [a]
toList (CVector v i m n) =
  V.toList (V.unsafeSlice i (m - i) v) ++ V.toList (V.unsafeTake i v)

-- | Convert to a list of given length with the selected element at the head.
-- Wrap around the bound. O(N).
toListN :: Int -> CVector a -> [a]
toListN k (CVector v i m n) = [v V.! ((i + j) `mod` m) | j <- [0 .. (k -1)]]

-- | Convert a list to a vector and select the first element. O(n).
--
-- The list must be non-empty.
fromList :: [a] -> CVector a
fromList [] = error "fromList: empty list"
fromList xs = CVector v 0 (V.length v) (V.length v) where v = V.fromList xs

-- -- | Take the first N elements from the list and convert them to a circular
-- -- vector. Select the first element. O(N).
-- --
-- -- Call error if list is too short.
-- fromListN :: Int -> [a] -> CVector a
-- fromListN n xs =
--   if n' == n
--     then fromList xs'
--     else error $ "fromListN: list is too short: " ++ show n'
--   where
--     xs' = take n xs
--     n' = length xs'

-- | Get selected element. O(1).
get :: CVector a -> a
get (CVector v i _ _) = V.unsafeIndex v i

-- -- | Get element with given offset from last element. Wrap around the bound.
-- -- O(1).
-- getAt :: Int -> CVector a -> a
-- getAt di (CVector v i m _) | di < 0 = error "getAt: negative offset"
--                            | otherwise = V.unsafeIndex v i'
--   where
--     i' = m - ( (i - di) `mod` m )

-- | Get selected element and select the previous one. O(1).
pop :: CVector a -> (a, CVector a)
pop c = (get c, previous c)

set :: Int -> a -> Vector a -> Vector a
set i x = V.modify (\v -> M.write v i x)

unsafeSet :: Int -> a -> Vector a -> Vector a
unsafeSet i x v = runST $ do
  mv <- V.unsafeThaw v
  M.unsafeWrite mv i x
  V.unsafeFreeze mv

-- | Replace the selected element. O(n).
put :: a -> CVector a -> CVector a
put x (CVector v i n) = CVector (set i x v) i n

-- | Replace the selected element. O(1).
--
-- Be careful; the internal vector is mutated! The immutable circular vector may
-- not be used after this operation. For example, we have
--
-- @
-- (c == unsafePut e c) = True
-- @
--
-- independent of @e@.
unsafePut :: a -> CVector a -> CVector a
unsafePut x (CVector v i n) = CVector (unsafeSet i x v) i n

-- | Replace the next element and shift the circular vector. O(n).
push :: a -> CVector a -> CVector a
push x c
  | size c == 1 = singleton x
  | otherwise = put x $ next c

-- | Replace the next element and shift the circular vector. O(1).
--
-- Be careful; the vector is mutated, see 'unsafePut'.
--
-- Further, @(pop . unsafePush)@ is not an identity because the next element was
-- changed.
unsafePush :: a -> CVector a -> CVector a
unsafePush x c
  | size c == 1 = singleton x
  | otherwise = unsafePut x $ next c

-- -- | Select the next element. If the bound is reached, the first element is
-- -- selected. O(1).
-- next :: CVector a -> CVector a
-- next (CVector v i n)
--   | i + 1 == n = CVector v 0 n
--   | otherwise = CVector v (i + 1) n

-- -- | Select the previous element. If the bound is reached, the last element is
-- -- selected. O(1).
-- previous :: CVector a -> CVector a
-- previous (CVector v i m n)
--   | i == 0 = CVector v (m - 1) n
--   | otherwise = CVector v (i - 1) n

-- -- | Select an element at a specific position. O(1).
-- select :: Int -> CVector a -> CVector a
-- select i (CVector v _ n)
--   | i >= n = error $ "select: index out of bounds: " ++ show (i, n)
--   | otherwise = CVector v i n

-- -- | Shift the selected element by given index. O(1).
-- shift :: Int -> CVector a -> CVector a
-- shift di (CVector v i n) = CVector v i' n
--   where
--     i' = (i + di) `mod` n
