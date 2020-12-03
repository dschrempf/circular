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
import qualified Data.Stack.Circular as C
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

-- When using foldl or foldl', list is much slower than cstack.

listFoldL ::  Int -> Int
listFoldL l = sum $ take 1000 $ foldl (flip (:)) [] [0..l]

cstackV ::  Int -> Int
cstackV l = runST $ do
  c <- C.replicate 1000 0 :: ST s (C.MStack V.Vector s Int)
  c' <- foldlM (flip C.push) c [0..l]
  C.sum c'

cstackU ::  Int -> Int
cstackU l = runST $ do
  c <- C.replicate 1000 0 :: ST s (C.MStack U.Vector s Int)
  c' <- foldlM (flip C.push) c [0..l]
  C.sum c'

-- When using foldr, cstack is slower by far. This is because list are lazy.

main :: IO ()
main = do
  let l = 1000000 :: Int
  print $ listFoldL l
  print $ cstackU l
  defaultMain
    [ bench "list, foldl" $ whnf listFoldL l
    , bench "cstack, foldl" $ whnf cstackV l
    , bench "cstack unboxed, foldl" $ whnf cstackU l ]

-- benchmarking list, foldl
-- time                 196.5 ms   (169.7 ms .. 219.9 ms)
--                      0.983 R²   (0.933 R² .. 1.000 R²)
-- mean                 213.9 ms   (197.8 ms .. 238.8 ms)
-- std dev              25.51 ms   (10.44 ms .. 37.57 ms)
-- variance introduced by outliers: 31% (moderately inflated)

-- benchmarking cstack, foldl
-- time                 18.65 ms   (18.11 ms .. 19.24 ms)
--                      0.993 R²   (0.983 R² .. 0.999 R²)
-- mean                 18.46 ms   (18.13 ms .. 18.99 ms)
-- std dev              979.7 μs   (565.1 μs .. 1.446 ms)
-- variance introduced by outliers: 21% (moderately inflated)

-- benchmarking cstack unboxed, foldl
-- time                 13.97 ms   (13.91 ms .. 14.05 ms)
--                      1.000 R²   (1.000 R² .. 1.000 R²)
-- mean                 13.98 ms   (13.95 ms .. 14.02 ms)
-- std dev              86.51 μs   (61.56 μs .. 120.9 μs)
