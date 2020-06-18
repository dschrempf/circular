{-# LANGUAGE RankNTypes #-}

{- |
Module      :  Data.Vector.Circular
Description :  Circular mutable vectors
Copyright   :  (c) Dominik Schrempf, 2020
License     :  GPL-3.0-or-later

Maintainer  :  dominik.schrempf@gmail.com
Stability   :  unstable
Portability :  portable

Creation date: Thu Jun 18 10:00:28 2020.

-}

module Data.Vector.Circular
  (
    -- * Boxed circular vectors
    CVector(vector, index, size)
  , -- * Construction
    empty
  , singleton
  , toVector
  , fromVector
  , toList
  , fromList
  , fromListN
  , -- * Accessors
    get
  , unsafePut
  , next
  , previous
  , select
  )
where

import           Control.Monad.ST

import qualified Data.Vector                   as V
import           Data.Vector                    ( Vector )
import qualified Data.Vector.Mutable           as M

-- | Circular mutable vectors are just normal vectors with a pointer to a
-- __selected element__.
data CVector a = CVector
  {
    vector :: Vector a
  , index  :: !Int
  , size   :: !Int
  }

-- | An empty circular vector; pretty useless. O(1).
empty :: CVector a
empty = CVector V.empty 0 0

-- | A circular vector with one element; still pretty useless. O(1).
singleton :: a -> CVector a
singleton x = CVector (V.singleton x) 0 1

-- | Copy a circular vector to a classical vector. The first element of the
-- returned vector is the current __selected element__. O(n).
toVector :: CVector a -> Vector a
toVector (CVector v i n) = V.unsafeSlice i (n - i) v V.++ V.unsafeTake i v

-- | Select the first element. O(1).
fromVector :: Vector a -> CVector a
fromVector v = CVector v 0 (V.length v)

-- | Convert to a list with the selected element at the head. O(n).
toList :: CVector a -> [a]
toList (CVector v i n) =
  V.toList (V.unsafeSlice i (n - i) v) ++ V.toList (V.unsafeTake i v)

-- | Convert a list to a vector and select the first element. O(n).
fromList :: [a] -> CVector a
fromList xs = CVector v 0 (V.length v) where v = V.fromList xs

-- | Take the first N elements from the list and convert them to a circular
-- vector. Select the first element. O(N).
--
-- Call error if list is too short.
fromListN :: Int -> [a] -> CVector a
fromListN n xs = if n' == n
  then fromList xs'
  else error $ "fromListN: list is too short: " ++ show n'
 where
  xs' = take n xs
  n'  = length xs'

-- | Get selected element. O(1).
get :: CVector a -> a
get (CVector v i _) = V.unsafeIndex v i

-- Replace vector.
replace :: Vector a -> CVector a -> CVector a
replace v (CVector _ i n) = CVector v i n

-- | Replace the selected element. O(1).
--
-- Be careful; the vector is mutated!
unsafePut :: a -> CVector a -> CVector a
unsafePut x c@(CVector v i _) = replace v' c
  where v' = runST $ do
          mv <- V.unsafeThaw v
          M.unsafeWrite mv i x
          V.unsafeFreeze mv

-- | Select the next element. If the bound is reached, the first element is
-- selected. O(1).
next :: CVector a -> CVector a
next (CVector v i n) | i + 1 == n = CVector v 0 n
                     | otherwise  = CVector v (i + 1) n

-- | Select the previous element. If the bound is reached, the last element is
-- selected. O(1).
previous :: CVector a -> CVector a
previous (CVector v i n) | i == 0    = CVector v (n - 1) n
                         | otherwise = CVector v (i - 1) n

-- | Select an element at a specific position. O(1).
select :: Int -> CVector a -> CVector a
select i (CVector v _ n)
  | i >= n    = error $ "select: index out of bounds: " ++ show (i, n)
  | otherwise = CVector v i n
