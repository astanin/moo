{-# LANGUAGE BangPatterns #-}
{- |

Common utility functions.

-}

module Moo.GeneticAlgorithm.Utilities
  (
  -- * Non-deterministic functions
    getRandomGenomes
  , withProbability
  -- * Common digest functions
  , avgFitness
  , maxFitness
  , minFitness
  , stdDeviation
) where

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Statistics (average, variance)

import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64
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


-- |Clip variable @v@ to stay within range @(vmin, vmax)@ (inclusive).
clip :: (Ord a) => (a, a) -> a -> a
clip range v =
    let vmin = uncurry min range
        vmax = uncurry max range
    in  max (min v vmax) vmin

-- |Returns the average fitnesses in a population.
avgFitness :: Population a -> Fitness
avgFitness = average . map snd

-- |Returns the maximum fitness in a population.
maxFitness :: Population a -> Fitness
maxFitness = maximum . map snd

-- |Returns the minimum fitness in a population.
minFitness :: Population a -> Fitness
minFitness = minimum . map snd

-- |Returns the standard deviation of the fitness values in a population.
stdDeviation :: Population a -> Double
stdDeviation = sqrt . variance . map snd

