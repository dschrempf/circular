{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}

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
--
-- Construction of mutable circular stacks is done with 'replicate' and subsequent
-- 'push'es, or with 'fromVector'. Use the data constructors for 'MStack' and
-- 'Stack' only if you know what you are doing.
--
-- When denoting the asymptotic runtime of functions, @n@ refers to the circular
-- stack size.
module Data.Stack.Circular
  ( -- * Circular stacks
    MStack (..),
    Stack (..),

    -- * Construction
    replicate,

    -- * Conversion
    fromVector,
    toVector,
    take,
    thaw,
    freeze,

    -- * Accessors
    get,
    pop,
    push,

    -- * Folds

    -- | __Commutativity__ of the combining function is __assumed__ for
    -- fold-like functions provided in this module, that is, the order of
    -- elements of the stack must not matter!
    foldl,
    sum,
    product,
  )
where

import Control.Monad.Primitive
import Data.Aeson
import Data.Aeson.TH
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as VG
import qualified Data.Vector.Generic.Mutable as VM
import Prelude hiding (foldl, product, replicate, sum, take)

-- | Mutable circular stacks with fixed size are just mutable vectors with a
-- pointer to the last element.
data MStack v s a = MStack
  { mStack :: VG.Mutable v s a,
    mIndex :: !Int
  }

-- | Immutable circular stack; useful, for example, to save, or restore a
-- mutable circular stack.
data Stack v a = Stack
  { iStack :: v a,
    iIndex :: !Int
  }
  deriving (Eq, Read, Show)

$(return [])

instance (FromJSON (v a)) => FromJSON (Stack v a) where
  parseJSON = $(mkParseJSON defaultOptions ''Stack)

instance (ToJSON (v a)) => ToJSON (Stack v a) where
  toJSON = $(mkToJSON defaultOptions ''Stack)
  toEncoding = $(mkToEncoding defaultOptions ''Stack)

-- | A circular stack of given size with the same element replicated.
--
-- Call 'error' if the maximum size is zero or negative.
--
-- O(n).
replicate :: (VG.Vector v a, PrimMonad m) => Int -> a -> m (MStack v (PrimState m) a)
replicate n x
  | n <= 0 = error "empty: maximum size must be one or larger"
  | otherwise = do
    v <- VM.replicate n x
    return $ MStack v 0

-- | Convert a vector to a circular stack with size being equal to the length of
-- the vector. The first element of the vector is the deepest (oldest) element
-- of the stack, the last element of the vector is the current (newest) element
-- of the stack.
--
-- The vector must be non-empty.
--
-- O(n).
fromVector :: (VG.Vector v a, PrimMonad m) => v a -> m (MStack v (PrimState m) a)
fromVector v
  | n == 0 = error "fromVector: empty vector"
  | otherwise = do
    mv <- VG.thaw v
    return $ MStack mv (n - 1)
  where
    n = VG.length v

-- | Convert a circular stack to a vector. The first element of the returned
-- vector is the deepest (oldest) element of the stack, the last element of the
-- returned vector is the current (newest) element of the stack.
--
-- O(n).
toVector :: VG.Vector v a => Stack v a -> v a
toVector (Stack v i) = VG.unsafeDrop (i + 1) v VG.++ VG.unsafeTake (i + 1) v

-- | Convert the last k elements of a circular stack to a vector. The first
-- element of the returned vector is the deepest (oldest) element of the stack,
-- the last element of the returned vector is the current (newest) element of
-- the stack.
--
-- The size of the stack must be larger than k.
--
-- O(k).
take :: (VG.Vector v a, PrimMonad m) => Int -> MStack v (PrimState m) a -> m (v a)
take k (MStack v i)
  | k < 0 = error "toVectorN: negative k"
  | k > n = error "toVectorN: circular stack too small"
  | k == 0 = return VG.empty
  | i0 == 0 = VG.freeze $ VM.unsafeTake k v
  | i0 + k <= n = VG.freeze $ VM.unsafeSlice i0 k v
  | otherwise = do
    l <- VG.freeze (VM.unsafeDrop (i + 1) v)
    r <- VG.freeze (VM.unsafeTake k' v)
    return $ l VG.++ r
  where
    n = VM.length v
    -- Starting index.
    i0 = (i + 1) `mod` n
    -- Number of elements already taken from the starting index to the end of the vector.
    dk = n - i0
    -- Number of elements we still have to take.
    k' = k - dk

-- | Conversion from immutable to mutable circular stack.
--
-- O(n).
thaw :: (VG.Vector v a, PrimMonad m) => Stack v a -> m (MStack v (PrimState m) a)
thaw (Stack v i) = do
  mv <- VG.thaw v
  return $ MStack mv i

-- | Conversion from mutable to immutable circular stack.
--
-- O(n).
freeze :: (VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m (Stack v a)
freeze (MStack mv i) = do
  v <- VG.freeze mv
  return $ Stack v i

-- Select the previous element without changing the stack.
previous :: VG.Vector v a => MStack v s a -> MStack v s a
previous (MStack v i) = MStack v i'
  where
    j = i - 1
    i' = if j < 0 then VM.length v - 1 else j

-- | Get the last element without changing the stack.
--
-- O(1).
get :: (VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m a
get (MStack v i) = VM.unsafeRead v i
{-# INLINE get #-}

-- | Pop the current element from the stack and put the focus on the previous
-- element.
--
-- Be careful: `pop` always succeeds, even if there are actually no more
-- elements on the stack (similar to walking in a circle).
--
-- O(1).
pop :: (VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m (a, MStack v (PrimState m) a)
pop x = do
  val <- get x
  return (val, previous x)

-- Replace the current element.
put :: (VG.Vector v a, PrimMonad m) => a -> MStack v (PrimState m) a -> m (MStack v (PrimState m) a)
put x (MStack v i) = VM.unsafeWrite v i x >> return (MStack v i)

-- Select the next element without changing the stack.
next :: VG.Vector v a => MStack v s a -> MStack v s a
next (MStack v i) = MStack v i'
  where
    i' = (i + 1) `mod` VM.length v

-- | Push an element on the stack.
--
-- O(1).
push :: (VG.Vector v a, PrimMonad m) => a -> MStack v (PrimState m) a -> m (MStack v (PrimState m) a)
push x = put x . next

-- Left fold over a mutable vector. This is all a little stupid.
foldlMV :: (VM.MVector v b, PrimMonad m) => (a -> b -> a) -> a -> v (PrimState m) b -> m a
foldlMV f x v = F.foldlM (\acc i -> f acc <$> VM.unsafeRead v i) x [0 .. (n -1)]
  where
    n = VM.length v

-- | Left fold.
--
-- O(n).
foldl :: (VG.Vector v b, PrimMonad m) => (a -> b -> a) -> a -> MStack v (PrimState m) b -> m a
foldl f x (MStack v _) = foldlMV f x v

-- | Compute the sum of the elements on the stack.
--
-- O(n).
sum :: (Num a, VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m a
sum = foldl (+) 0

-- | Compute the product of the elements on the stack.
--
-- O(n).
product :: (Num a, VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m a
product = foldl (*) 1
