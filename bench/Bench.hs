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
import Data.Vector.Unboxed (Vector)

-- When using foldl or foldl', list is much slower than cstack.

list ::  Int -> Int
list l = sum $ take 1000 $ foldl (flip (:)) [] [0..l]

cstack ::  Int -> Int
cstack l = S.sum $ foldl (flip S.unsafePush) (S.empty 1000 :: S.CStack Vector Int) [0..l]

-- -- When using foldr, cstack is slower by far. This is because of the
-- -- lazyness. However, for stacks, by definition, the last added elements are
-- -- of interest.

-- listR ::  Int -> Int
-- listR l = sum $ take 1000 $ foldr (:) [] [0..l]

-- cstackR ::  Int -> Int
-- cstackR l = S.sum $ foldr S.unsafePush (S.empty 1000 :: S.CStack Vector Int) [0..l]

main :: IO ()
main = do
  let l = 1000000 :: Int
  print $ list l
  print $ cstack l
  defaultMain
    [ bench "list, foldl" $ whnf list l
    , bench "cstack, foldl" $ whnf cstack l ]
    -- , bench "list, foldr" $ whnf listR l
    -- , bench "cstack, foldr" $ whnf cstackR l ]
