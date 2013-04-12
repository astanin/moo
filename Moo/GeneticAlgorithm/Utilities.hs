{-# LANGUAGE BangPatterns #-}
{- |

Common utility functions.

-}

module Moo.GeneticAlgorithm.Utilities
  (
  -- * Non-deterministic functions
    getRandomGenomes
  , doCrossovers
  , doNCrossovers
) where

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random

import Control.Monad.Mersenne.Random
import Control.Monad (replicateM)

-- | Generate @n@ random genomes of length @len@ made of elements
-- in the range @(from,to)@. Return a list of genomes and a new state of
-- random number generator.
randomGenomes :: (Random a, Ord a)
              => PureMT -> Int -> Int -> (a, a) ->  ([Genome a], PureMT)
randomGenomes rng n len (from, to) =
    let lo = min from to
        hi = max from to
    in flip runRandom rng $
        replicateM n $ replicateM len $ getRandomR (lo,hi)

-- | Generate @n@ random genomes of length @len@ made of elements
-- in the range @(from,to)@. Return a list of genomes.
getRandomGenomes :: (Random a, Ord a)
                 => Int -- ^ @n@, how many genomes to generate
                 -> Int -- ^ @len@, genome length
                 ->  (a, a) -- ^ @range@ of genome bit values
                 -> Rand ([Genome a])
getRandomGenomes n len range = Rand $ \rng ->
                               let (gs, rng') = randomGenomes rng n len range
                               in  R gs rng'

-- | Crossover all available parents. Parents are not repeated.
doCrossovers :: [Genome a] -> CrossoverOp a -> Rand [Genome a]
doCrossovers []      _     = return []
doCrossovers parents xover = do
  (children', parents') <- xover parents
  rest <- doCrossovers parents' xover
  return $ children' ++ rest


-- | Produce exactly @n@ offsprings by repeatedly running the @crossover@
-- operator between randomly selected parents (possibly repeated).
doNCrossovers :: Int   -- ^ @n@, number of offsprings to generate
              -> [Genome a]  -- ^ @parents@' genomes
              -> CrossoverOp a  -- ^ @crossover@ operator
              -> Rand [Genome a]
doNCrossovers _ [] _ = return []
doNCrossovers n parents xover =
    doAnotherNCrossovers n []
  where
    doAnotherNCrossovers i children
        | i <= 0     = return . take n . concat $ children
        | otherwise  = do
      (children', _) <- xover =<< shuffle parents
      doAnotherNCrossovers (i - length children') (children':children)
