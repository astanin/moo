{-# LANGUAGE BangPatterns #-}
{- |

Common utility functions.

-}

module Moo.GeneticAlgorithm.Utilities
  (
  -- * Non-deterministic functions
    getRandomGenomes
  , withProbability
) where

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random

import Control.Monad.Mersenne.Random
import Control.Monad (liftM, replicateM)

-- |Modify value with probability @p@.
withProbability :: Double -> a -> (a -> Rand a) -> Rand a
withProbability p x modify = do
  t <- getDouble
  if t < p
     then modify x
     else return x

-- | Generate @n@ random genomes of length @len@ made of elements
-- in the range @(from,to)@. Return a list of genomes and a new state of
-- random number generator.
randomGenomes :: (Enum a) => PureMT -> Int -> Int -> (a, a) ->  ([Genome a], PureMT)
randomGenomes rng n len (from, to) =
    let lo = fromEnum from
        hi = fromEnum to
    in flip runRandom rng $
       (nLists len . map toEnum) `liftM` replicateM (n*len) (getRandomR (lo,hi))
  where nLists :: Int -> [a] -> [[a]]
        nLists _ [] = []
        nLists n ls = let (h,t) = splitAt n ls in h : nLists n t

-- | Generate @n@ random genomes of length @len@ made of elements
-- in the range @(from,to)@. Return a list of genomes.
getRandomGenomes :: (Enum a)
                 => Int -- ^ how many genomes to generate
                 -> Int -- ^ genome length
                 ->  (a, a) -- ^ range of genome bit values
                 -> Rand ([Genome a])
getRandomGenomes n len range = Rand $ \rng ->
                               let (gs, rng') = randomGenomes rng n len range
                               in  R gs rng'
