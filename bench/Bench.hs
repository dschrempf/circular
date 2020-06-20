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
import Data.List
import qualified Data.Stack.Circular as S
import Data.Vector.Unboxed (Vector)

list ::  Int -> Int
list l = sum $ take 1000 $ foldl' (flip (:)) [] [0..l]

cstack ::  Int -> Int
cstack l = S.sum $ foldl' (flip S.unsafePush) (S.empty 1000 :: S.CStack Vector Int) [0..l]

main :: IO ()
main = do
  let l = 1000000 :: Int
  print $ list l
  print $ cstack l
  defaultMain
    [ bench "list" $ whnf list l
    , bench "cstack" $ whnf cstack l ]
