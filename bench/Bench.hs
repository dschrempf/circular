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

import Criterion.Main
import qualified Data.Stack.Circular as S
import qualified Data.Vector.Unboxed as U
import qualified Data.Vector as V

-- When using foldl or foldl', list is much slower than cstack.

list ::  Int -> Int
list l = sum $ take 1000 $ foldl (flip (:)) [] [0..l]

cstackV ::  Int -> Int
cstackV l = S.sum $ foldl (flip S.unsafePush) (S.unsafeEmpty 1000 :: S.CStack V.Vector Int) [0..l]

cstackU ::  Int -> Int
cstackU l = S.sum $ foldl (flip S.unsafePush) (S.unsafeEmpty 1000 :: S.CStack U.Vector Int) [0..l]

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
