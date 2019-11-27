{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}

module Fibonacci where

import Control.Comonad.Cofree
import Data.Functor.Foldable
import GHC.Natural

import BinTree (BinTree)
import BinTree (BinTreeF)
import qualified BinTree

fibonacciHylo :: Int -> Int
fibonacciHylo = hylo fibonacciAlg fibonacciCoalg

fibonacciHylo' :: Int -> Int
fibonacciHylo' = cata fibonacciAlg . id @(Fix (BinTreeF Int)) . ana fibonacciCoalg

fibonacciHisto :: Natural -> Int
fibonacciHisto = histo (\case
                           (Just (a :< (Just (b :< _)))) -> a + b
                           _ -> 1)

fibonacciAlg :: BinTreeF Int Int -> Int
fibonacciAlg (BinTree.LeafF a) = a
fibonacciAlg (BinTree.BranchF a b) = a + b

fibonacciCoalg :: Int -> BinTreeF Int Int
fibonacciCoalg n
  | n <= 1 = BinTree.LeafF n
  | otherwise = BinTree.BranchF (n - 1) (n - 2)

fibonacciTree :: Int -> BinTree Int
fibonacciTree = ana fibonacciCoalg
