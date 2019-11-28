{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad ((<=<), (>=>))
import Data.Functor.Foldable (Base, Corecursive (embed), Fix (Fix), Recursive, cata, para)

data IDA q a f = Answer q a | Decomp q [f]
  deriving (Foldable, Functor, Traversable)

type IDATree q a = Fix (IDA q a)

newtype Q = Q { unQ :: String }
newtype A = A { unA :: String }

main :: IO ()
main =
  putStrLn "What's the overall question?" >>
  Q <$> getLine >>=
  ida >>=
  putStrLn . ("The answer is: " <>) . unA

ida :: Q -> IO A
ida = aggregate <=< traverse' answer <=< decompose

decompose :: Q -> IO (IDATree Q a)
decompose = anaM coalg

aggregate :: IDATree Q A -> IO A
aggregate = paraM alg

answer :: IDA Q A f -> IO (IDA Q A f)
answer (Decomp (Q q) []) = putStrLn "What's the answer?" >> putStrLn q >> (Answer (Q q) . A <$> getLine)
answer x = pure x

alg :: IDA Q A (Fix (IDA Q A), A) -> IO A
alg (Answer _ a) = pure a
alg (Decomp (Q q') qs') =
  putStrLn ("Original question: " <> q') >>
  putStrLn "Subquestions and subanswers:" >>
  traverse display qs' >>
  putStrLn "What's the answer?" >>
  A <$> getLine
  where
    display (Fix (Decomp (Q q) _), A a) = putStrLn ("Q: " <> q <> "\nA: " <> a)
    display (Fix (Answer (Q q) _), A a) = putStrLn ("Q: " <> q <> "\nA: " <> a)

traverse' :: Monad m => (forall f. IDA q a f -> m (IDA q' a' f)) -> Fix (IDA q a) -> m (Fix (IDA q' a'))
traverse' f = cataM (fmap Fix . f)

coalg :: Q -> IO (IDA Q a Q)
coalg (Q q') =
  putStrLn ("Q: " <> q') >>
  inquire "Can you immediately answer this question?" >>=
    \case
      True -> pure $ Decomp (Q q') []
      False -> Decomp (Q q') <$> subqs First
  where
    recurse s = do
      putStrLn s
      q <- getLine
      (Q q :) <$> subqs Not
    subqs Not =
      inquire "Any more subquestions?" >>=
        \case
          True -> recurse "What's the next subquestion then?"
          False -> pure []
    subqs First =
      recurse "What's the first subquestion then?"

data FirstOrNot = First | Not

inquire :: String -> IO Bool
inquire s = do
  putStrLn (s <> " y/n")
  res <- getLine
  pure $ res == "y" || res == "Y"

cataM :: (Traversable (Base t), Monad m, Recursive t) => (Base t c -> m c) -> t -> m c
cataM = cata . (sequence >=>)

paraM :: (Traversable (Base t), Monad m, Recursive t) => (Base t (t, c) -> m c) -> t -> m c
paraM f = para (sequence . fmap pack >=> f)
  where
    pack (a, mb) = (a,) <$> mb

anaM :: (Corecursive t, Traversable (Base t), Monad m) => (a -> m (Base t a)) -> a -> m t
anaM g = a
  where
    a = pure . embed <=< traverse a <=< g
