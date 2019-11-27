{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

module Main where

import Control.Monad ((<=<), (>=>))
import Data.Functor.Foldable (Base, Corecursive (embed), Fix (Fix), Recursive, para)

data IDA q a f = Answer q a | Decomp q [f]
  deriving (Foldable, Functor, Traversable)

main :: IO ()
main =
  putStrLn "What's the overall question?" >>
  getLine >>=
  ida >>=
  putStrLn . ("The answer is: " <>)

ida :: String -> IO String
ida = paraM idaAlg <=< anaM idaCoalg

idaAlg :: IDA String String (Fix (IDA String String), String) -> IO String
idaAlg (Answer _ a) = pure a
idaAlg (Decomp q' qs') =
  putStrLn ("Original question: " <> q') >>
  putStrLn "Subquestions and subanswers:" >>
  traverse display qs' >>
  putStrLn "What's the answer?" >>
  getLine
  where
    display (Fix (Decomp q _), a) = putStrLn ("Q: " <> q <> "\nA: " <> a)
    display (Fix (Answer q _), a) = putStrLn ("Q: " <> q <> "\nA: " <> a)

idaCoalg :: String -> IO (IDA String String String)
idaCoalg q' =
  putStrLn ("Q: " <> q') >>
  inquire "Can you immediately answer this question?" >>=
    \case
      True -> putStrLn "What's the answer?" >> (Answer q' <$> getLine)
      False -> Decomp q' <$> subqs First
  where
    recurse s = do
      putStrLn s
      q <- getLine
      (q :) <$> subqs Not
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

paraM :: (Traversable (Base t), Monad m, Recursive t) => (Base t (t, c) -> m c) -> t -> m c
paraM f = para (sequence . fmap pack >=> f)
  where
    pack (a, mb) = (a,) <$> mb

anaM :: (Corecursive t, Traversable (Base t), Monad m) => (a -> m (Base t a)) -> a -> m t
anaM g = a
  where
    a = pure . embed <=< traverse a <=< g
