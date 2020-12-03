-- |
-- Module      :  Main
-- Description :  Benchmark circular stacks
-- Copyright   :  (c) Dominik Schrempf, 2020
-- License     :  GPL-3.0-or-later
--
-- Maintainer  :  dominik.schrempf@gmail.com
-- Stability   :  unstable
-- Portability :  portable
--
-- Creation date: Sat Jun 20 21:12:38 2020.
module Main
  ( main
  )
where

import Control.Monad.ST
import Criterion.Main
import Data.Foldable
import qualified Data.Stack.Circular as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

-- When using foldl or foldl', list is much slower than cstack.

list ::  Int -> Int
list l = sum $ take 1000 $ foldl (flip (:)) [] [0..l]

cstackV ::  Int -> Int
cstackV l = runST $ do
  c <- S.empty 1000 :: ST s (S.MCStack V.Vector s Int)
  c' <- foldlM (flip S.push) c [0..l]
  S.sum c'

cstackU ::  Int -> Int
cstackU l = runST $ do
  c <- S.empty 1000 :: ST s (S.MCStack U.Vector s Int)
  c' <- foldlM (flip S.push) c [0..l]
  S.sum c'

-- When using foldr, cstack is slower by far. This is because of the
-- lazyness. However, for stacks, by definition, the last added elements are
-- of interest.

-- -- The safe operations are very slow.

-- cstackVSafe ::  Int -> Int
-- cstackVSafe l = S.sum $ foldl (flip S.push) (S.empty 1000 :: S.CStack V.Vector Int) [0..l]

-- cstackUSafe ::  Int -> Int
-- cstackUSafe l = S.sum $ foldl (flip S.push) (S.empty 1000 :: S.CStack U.Vector Int) [0..l]


main :: IO ()
main = do
  let l = 1000000 :: Int
  print $ list l
  print $ cstackU l
  defaultMain
    [ bench "list, foldl" $ whnf list l
    , bench "cstack, foldl" $ whnf cstackV l
    , bench "cstack unboxed, foldl" $ whnf cstackU l ]

-- benchmarking list, foldl
-- time                 189.5 ms   (167.1 ms .. 211.6 ms)
--                      0.987 R²   (0.972 R² .. 1.000 R²)
-- mean                 206.4 ms   (191.3 ms .. 245.1 ms)
-- std dev              31.05 ms   (5.684 ms .. 44.68 ms)
-- variance introduced by outliers: 47% (moderately inflated)

-- benchmarking cstack, foldl
-- time                 121.1 ms   (101.2 ms .. 144.9 ms)
--                      0.964 R²   (0.940 R² .. 0.995 R²)
-- mean                 106.4 ms   (101.3 ms .. 114.2 ms)
-- std dev              10.60 ms   (6.176 ms .. 16.09 ms)
-- variance introduced by outliers: 31% (moderately inflated)

-- benchmarking cstack unboxed, foldl
-- time                 87.88 ms   (78.67 ms .. 92.92 ms)
--                      0.990 R²   (0.973 R² .. 1.000 R²)
-- mean                 97.01 ms   (92.03 ms .. 106.9 ms)
-- std dev              10.14 ms   (1.098 ms .. 13.61 ms)
-- variance introduced by outliers: 32% (moderately inflated)
