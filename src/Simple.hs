{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE RankNTypes #-}

module Simple where

import Data.Functor.Foldable (Fix (Fix), ana, cata)

--- A question `q` can either have an answer `a` or be decomposed into further pieces `f`
data IDA q a f = Answer q a | Decomp q [f]
  deriving (Foldable, Functor, Traversable)

--- Overall, a question decomposition creates a rose tree (by taking the fixed point).
type IDATree q a = Fix (IDA q a)

-- A `Q` is a simple wrapper over a `String` meant to suggest that the string is a question.
newtype Q = Q { unQ :: String }
-- An `A` is a simple wrapper over a `String` meant to suggest that the string is an answer.
newtype A = A { unA :: String }

-- Overall, IDA takes a question as input and renders an answer.
-- It does this by: (1) decomposing the question, (2) answering simple questions, and (3) aggregating the results.
ida :: Q -> A
ida = aggregateFully . fmap' answer . decomposeFully

-- Turns a high-level question into a decomposed tree of simpler questions.
decomposeFully :: Q -> IDATree Q a
decomposeFully = ana decompose

-- Turns a tree of answered, simple questions into an answer to the original question.
aggregateFully :: IDATree Q A -> A
aggregateFully = cata aggregate

-- Answer the simplest, leaf questions.
answer :: IDA Q A f -> IDA Q A f
answer = undefined

-- Break down a question into simpler subquestions.
decompose :: Q -> IDA Q a Q
decompose = undefined

-- Answer a question with the help of the subquestions and answers.
aggregate :: IDA Q A A -> A
aggregate = undefined

fmap' :: (forall f. IDA q a f -> IDA q' a' f) -> Fix (IDA q a) -> Fix (IDA q' a')
fmap' f = cata (Fix . f)
