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
  ( -- * Mutable circular stacks
    MStack (..),

    -- ** Construction and conversion
    replicate,
    fromVector,
    toVector,
    take,

    -- ** Accessors
    get,
    pop,
    push,

    -- ** Folds

    -- | __Commutativity__ of the combining function is __assumed__ for
    -- fold-like functions provided in this module, that is, the order of
    -- elements of the stack must not matter!
    foldl,
    sum,
    product,

    -- * Immutable circular stacks
    Stack (..),
    thaw,
    freeze,
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
toVector :: (VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m (v a)
toVector (MStack v i) = do
  l <- VG.freeze $ VM.unsafeDrop i' v
  r <- VG.freeze $ VM.unsafeTake i' v
  return $ l VG.++ r
  where i' = i+1

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
  -- We know now that k is in [1, n] and check if all k elements can be taken in
  -- one go.
  | i0 >= 0 = VG.freeze $ VM.unsafeSlice i0 k v
  -- Now we now that i0 is negative.
  | otherwise = do
    -- The length of r is i'.
    r <- VG.freeze $ VM.unsafeTake i' v
    -- The length of l has to be k-i'. So we have to drop n-(k-i')=n+i0.
    l <- VG.freeze $ VM.unsafeDrop (n+i0) v
    return $ l VG.++ r
  where
    n = VM.length v
    i' = i + 1
    -- The starting index. Can be negative.
    i0 = i' - k

-- | Get the last element without changing the stack.
--
-- O(1).
get :: (VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m a
get (MStack v i) = VM.unsafeRead v i
{-# INLINE get #-}

-- Select the previous element without changing the stack.
previous :: VG.Vector v a => MStack v s a -> MStack v s a
previous (MStack v i) = MStack v i'
  where
    j = i - 1
    i' = if j < 0 then VM.length v - 1 else j

-- | Pop the current element from the stack and put the focus on the previous
-- element.
--
-- Be careful:
--
-- - The stack is always full! Popping returns the last element and moves the
--   index to the second-last element, but the element is not truly removed from
--   the stack. It is only put to the end of the queue.
--
-- - Hence, `pop` always succeeds, even if there are actually no more elements
--   on the stack (similar to walking backwards in a circle).
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

-- | Left fold over all elements of the stack.
--
-- Please see the documentation of 'pop'.
--
-- O(n).
foldl :: (VG.Vector v b, PrimMonad m) => (a -> b -> a) -> a -> MStack v (PrimState m) b -> m a
foldl f x (MStack v _) = foldlMV f x v

-- | Compute the sum of the elements on the stack.
--
-- Please see the documentation of 'pop'.
--
-- O(n).
sum :: (Num a, VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m a
sum = foldl (+) 0

-- | Compute the product of the elements on the stack.
--
-- Please see the documentation of 'pop'.
--
-- O(n).
product :: (Num a, VG.Vector v a, PrimMonad m) => MStack v (PrimState m) a -> m a
product = foldl (*) 1

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
