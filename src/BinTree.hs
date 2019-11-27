{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE TypeFamilies #-}

module BinTree where

import Data.Functor.Foldable

data BinTree a = Leaf a | Branch (BinTree a) (BinTree a)
  deriving (Functor, Show)
data BinTreeF a f = LeafF a | BranchF f f
  deriving (Functor, Show)
type instance Base (BinTree a) = BinTreeF a
instance Recursive (BinTree a) where
  project (Leaf x) = LeafF x
  project (Branch a b) = BranchF a b
instance Corecursive (BinTree a) where
  embed (LeafF x) = Leaf x
  embed (BranchF a b) = Branch a b

