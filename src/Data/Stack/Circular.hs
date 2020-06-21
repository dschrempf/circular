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
--
-- Construction of a mutable circular stack 'MCStack' is done with 'empty' and
-- subsequent 'push'es, or the provided type conversion functions so that the
-- index and bounds are updated and checked consistently.
--
-- Immutable circular stacks are provided, for example, to save, or restore a
-- mutable circular stack.
--
-- The data constructors for 'MCStack' and 'CStack' are exported only to enable
-- creation of orphan instances such as Arbitrary (QuickCheck).
--
--
-- When denoting the efficiency of the functions @m@ refers to the current size
-- of the stack, and @n@ to the maximum size.
module Data.Stack.Circular
  ( -- * Circular stacks
    MCStack (..),
    CStack (..),

    -- * Construction
    empty,

    -- * Conversion
    toVector,
    toVectorN,
    fromVector,
    thaw,
    freeze,

    -- * Accessors
    get,
    pop,
    push,
    reset,

    -- * Queries
    isFull,

    -- * Folding

    -- | Here all fold functions should be provided, but I am too lazy. Instead,
    -- let's just provide some optimized functions to compute summary statistics
    -- across all values on the stack.
    --
    -- For reasons of efficiency, __commutativity__ of the combining function
    -- is __assumed__ for fold-like functions provided in this section! That is,
    -- the order of elements of the stack must not matter.

    foldl,
    sum,
    product,
  )
where

import Control.Monad.Primitive
import Control.Monad.ST
import Data.Aeson
import Data.Aeson.Types
import qualified Data.Foldable as F
import qualified Data.Vector.Generic as V
import Data.Vector.Generic (Mutable, Vector)
import qualified Data.Vector.Generic.Mutable as M
import Data.Vector.Generic.Mutable (MVector)
import Prelude hiding (foldl, product, sum)

-- | Mutable circular stacks with fixed maximum size are just mutable vectors
-- with a pointer to the last element.
data MCStack v s a = MCStack
  { mStack :: Mutable v s a,
    mIndex :: !Int,
    mSize :: !Int
  }

-- | Immutable circular stack; useful, for example, to save, or restore a
-- mutable circular stack.
data CStack v a = CStack
  { iStack :: v a,
    iIndex :: !Int,
    iSize :: !Int
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
      n = V.length $ iStack c
  toEncoding c = pairs ("stack" .= toVector c <> "maxSize" .= n)
    where
      n = V.length $ iStack c

instance (FromJSON a, FromJSON (v a), Vector v a) => FromJSON (CStack v a) where
  parseJSON = withObject "CStack" fromObject

fromObject :: forall v a. (FromJSON (v a), Vector v a) => Object -> Parser (CStack v a)
fromObject o = do
  v <- o .: "stack" :: Parser (v a)
  n <- o .: "maxSize" :: Parser Int
  let m = V.length v
      v' =
        if m < n
          then runST $ do
            mv <- V.unsafeThaw v :: ST s (Mutable v s a)
            mv' <- M.grow mv (n - m)
            V.unsafeFreeze mv'
          else v
  let i = if m < n then m -1 else n -1
  return $ CStack v' i m

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
empty :: (Vector v a, PrimMonad m) => Int -> m (MCStack v (PrimState m) a)
empty n
  | n <= 0 = error "empty: maximum size must be 1 or larger"
  | otherwise = do
    v <- M.unsafeNew n
    return $ MCStack v 0 0

-- -- | Convert a circular stack to a vector. The first element of the returned
-- -- vector is the deepest (oldest) element of the stack, the last element of the
-- -- returned vector is the current (newest) element of the stack.
-- --
-- -- This is a relatively expensive operation. O(m).
-- toVector :: (Vector v a, PrimMonad m) => MCStack v (PrimState m) a -> m (v a)
-- toVector (MCStack v i m)
--   | m == 0 = return V.empty
--   | i' + m <= n = V.freeze $ M.unsafeSlice i' m v
--   | otherwise = do
--       w <- M.new m
--       sequence_ [ M.unsafeRead v j >>= M.unsafeWrite w (j-i') | j <- [i' .. (n-1)] ]
--       sequence_ [ M.unsafeRead v j >>= M.unsafeWrite w (j+(n-1 - i'))  | j <- [0 .. i] ]
--       V.unsafeFreeze w
--   where
--     n = M.length v
--     i' = startIndex i m n

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
  | k < 0 = error "toVectorN: negative N"
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
fromVector :: (Vector v a, PrimMonad m) => v a -> m (MCStack v (PrimState m) a)
fromVector v
  | V.null v = error "fromVector: empty vector"
  | otherwise = do
    mv <- V.thaw v
    return $ MCStack mv (n - 1) n
  where
    n = V.length v

-- | Conversion from immutable to mutable circular stack. O(m).
thaw :: (Vector v a, PrimMonad m) => CStack v a -> m (MCStack v (PrimState m) a)
thaw (CStack v i m) = do
  mv <- V.thaw v
  return $ MCStack mv i m

-- -- For internal use.
-- unsafeThaw :: (Vector v a, PrimMonad m) => CStack v a -> m (MCStack v (PrimState m) a)
-- unsafeThaw (CStack v i m) = do
--   mv <- V.unsafeThaw v
--   return $ MCStack mv i m

-- | Conversion from mutable to immutable circular stack. O(m).
freeze :: (Vector v a, PrimMonad m) => MCStack v (PrimState m) a -> m (CStack v a)
freeze (MCStack mv i m) = do
  v <- V.freeze mv
  return $ CStack v i m

-- -- For internal use.
-- unsafeFreeze :: (Vector v a, PrimMonad m) => MCStack v (PrimState m) a -> m (CStack v a)
-- unsafeFreeze (MCStack mv i m) = do
--   v <- V.freeze mv
--   return $ CStack v i m

-- | Get the last element without changing the stack. O(1).
get :: (Vector v a, PrimMonad m) => MCStack v (PrimState m) a -> m a
get (MCStack v i _) = M.unsafeRead v i
{-# INLINE get #-}

-- Select the previous element without changing the stack.
previous :: Vector v a => MCStack v s a -> MCStack v s a
previous (MCStack v i m)
  | m == 0 = error "previous: empty stack"
  | i == 0 = MCStack v (n - 1) (m - 1)
  | otherwise = MCStack v (i - 1) (m - 1)
  where
    n = M.length v

-- | Get the last element and remove it from the stack. O(1).
--
-- The stack must be non-empty.
pop :: (Vector v a, PrimMonad m) => MCStack v (PrimState m) a -> m (a, MCStack v (PrimState m) a)
pop c = do
  x <- get c
  return (x, previous c)
{-# INLINE pop #-}

-- Replace the current element.
put :: (Vector v a, PrimMonad m) => a -> MCStack v (PrimState m) a -> m (MCStack v (PrimState m) a)
put x (MCStack v i m) = M.unsafeWrite v i x >> return (MCStack v i m)

-- Select the next element without changing the stack.
next :: Vector v a => MCStack v s a -> MCStack v s a
next (MCStack v i m)
  | i == (n - 1) = MCStack v 0 (min (m + 1) n)
  | otherwise = MCStack v (i + 1) (min (m + 1) n)
  where
    n = M.length v

-- | Push an element on the stack. Slow! If possible, use 'unsafePush'. O(n).
push :: (Vector v a, PrimMonad m) => a -> MCStack v (PrimState m) a -> m (MCStack v (PrimState m) a)
push x = put x . next

-- | Reset the stack. O(1).
reset :: MCStack v s a -> MCStack v s a
reset (MCStack v _ _) = MCStack v 0 0

-- | Check if the stack is full.
isFull :: Vector v a => MCStack v s a -> Bool
isFull (MCStack v _ m) = M.length v == m

-- Left fold over a mutable vector. This is all a little stupid.
foldlMV :: (MVector v b, PrimMonad m) => (a -> b -> a) -> a -> v (PrimState m) b -> m a
foldlMV f x v = F.foldlM (\acc i -> M.read v i >>= \e -> return (f acc e)) x [0..(l-1)]
  where l = M.length v

-- | (Monadic) left fold. O(m).
foldl :: (Vector v b, PrimMonad m) => (a -> b -> a) -> a -> MCStack v (PrimState m) b -> m a
foldl f x (MCStack v i m)
  | m == n = foldlMV f x v
  | i' + m <= n = foldlMV f x $ M.unsafeSlice i' m v
  | otherwise = do
      acc <- foldlMV f x $ M.unsafeDrop i' v
      foldlMV f acc $ M.unsafeTake (i + 1) v
  where
    n = M.length v
    i' = startIndex i m n

-- | Compute the sum of the elements on the stack. O(m).
sum :: (Num a, Vector v a, PrimMonad m) => MCStack v (PrimState m) a -> m a
sum = foldl (+) 0

-- | Compute the product of the elements on the stack. O(m).
product :: (Num a, Vector v a, PrimMonad m) => MCStack v (PrimState m) a -> m a
product = foldl (*) 1
