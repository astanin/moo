{- |

Common crossover operators for genetic algorithms.

-}

module Moo.GeneticAlgorithm.Crossover
  (
  -- ** Discrete operators
    onePointCrossover
  , twoPointCrossover
  , uniformCrossover
  , noCrossover
  -- ** Application
  , doCrossovers
  , doNCrossovers
) where

import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Utilities

import Control.Monad (liftM)

-- | Crossover two lists in exactly @n@ random points.
nPointCrossover :: Int -> ([a], [a]) -> Rand ([a], [a])
nPointCrossover n (xs,ys)
    | n <= 0 = return (xs,ys)
    | otherwise =
  let len = min (length xs) (length ys)
  in  do
    pos <- getRandomR (0, len-n)
    let (hxs, txs) = splitAt pos xs
    let (hys, tys) = splitAt pos ys
    (rxs, rys) <- nPointCrossover (n-1) (tys, txs) -- FIXME: not tail recursive
    return (hxs ++ rxs, hys ++ rys)

-- |Select a random point in two genomes, and swap them beyond this point.
-- Apply with probability @p@.
onePointCrossover :: Double -> CrossoverOp a
onePointCrossover _ []  = return ([],[])
onePointCrossover _ [celibate] = return ([],[celibate])
onePointCrossover p (g1:g2:rest) = do
  (h1,h2) <- withProbability p (nPointCrossover 1) (g1, g2)
  return ([h1,h2], rest)

-- |Select two random points in two genomes, and swap everything in between.
-- Apply with probability @p@.
twoPointCrossover :: Double -> CrossoverOp a
twoPointCrossover _ []  = return ([], [])
twoPointCrossover _ [celibate] = return ([],[celibate])
twoPointCrossover p (g1:g2:rest) = do
  (h1,h2) <- withProbability p (nPointCrossover 2) (g1,g2)
  return ([h1,h2], rest)

-- |Swap individual bits of two genomes with probability @p@.
uniformCrossover :: Double -> CrossoverOp a
uniformCrossover _ []  = return ([], [])
uniformCrossover _ [celibate] = return ([],[celibate])
uniformCrossover p (g1:g2:rest) = do
  (h1, h2) <- unzip `liftM` mapM swap (zip g1 g2)
  return ([h1,h2], rest)
  where
    swap = withProbability p (\(a,b) -> return (b,a))
