{-# LANGUAGE BangPatterns #-}
{- |

Helper functions to run genetic algorithms and control iterations.

-}

module Moo.GeneticAlgorithm.Run (
  -- * Running algorithm
    runGA
  , nextGeneration
  , evalFitness
  -- * Iteration control
  , loopUntil
  , writeEvery, ioEvery
  , Cond(..)
) where

import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Utilities (minFitness, maxFitness, avgFitness, stdDeviation)
import Moo.GeneticAlgorithm.Selection (sortByFitness)
import Moo.GeneticAlgorithm.Types

import Control.Monad.Writer.Lazy (WriterT, Monoid, tell, lift)

-- | Helper function to run an entire algorithm in the 'Rand' monad.
-- It takes care of generating a new random number generator.
runGA :: Rand a -> IO a
runGA ga = do
  rng <- newPureMT
  return $ evalRandom ga rng

-- | A single step of the genetic algorithm.
--
-- See "Moo.GeneticAlgorithm.Binary" and "Moo.GeneticAlgorithm.Continuous"
-- for the building blocks of the algorithm.
--
nextGeneration ::
    Int                   -- ^ @elite@, the number of genomes to keep intact
    -> FitnessFunction a  -- ^ fitness function
    -> SelectionOp a      -- ^ selection operator
    -> CrossoverOp a      -- ^ crossover operator
    -> MutationOp a       -- ^ mutation operator
    -> Population a       -- ^ current population
    -> Rand (Population a) -- ^ next generation
nextGeneration elite fitness selectOp xoverOp mutationOp pop = do
  genomes' <- withElite elite selectOp pop
  let top = take elite genomes'
  let rest = drop elite genomes'
  genomes' <- shuffle rest         -- just in case if selectOp preserves order
  genomes' <- doCrossovers genomes' xoverOp
  genomes' <- mapM mutationOp genomes'
  return $ evalFitness fitness (top ++ genomes')

-- | Select @n@ best genomes, then select more genomes from the
-- /entire/ population (elite genomes inclusive). Elite genomes will
-- be the first in the list.
withElite :: Int -> SelectionOp a -> SelectionOp a
withElite n select = \population -> do
  let elite = take n . eliteGenomes $ population
  selected <- select population
  return (elite ++ selected)
  where
    eliteGenomes = map fst . sortByFitness

-- | Run strict iterations of the genetic algorithm defined by @step@.
-- Termination condition @cond@ is evaluated before every step.
-- Return the result of the last step.
{-# INLINE loopUntil #-}
loopUntil :: (Monad m)
      => Cond a           -- ^ termination condition @cond@
      -> Population a     -- ^ initial population
      -> (Population a -> m (Population a))
                         -- ^ @step@ function to produce the next generation
      -> m (Population a) -- ^ final population
loopUntil cond pop0 step = go cond pop0
  where
    go cond !x
       | evalCond cond x  = return x
       | otherwise        = step x >>= go (countdownCond cond)

-- | Interleave monadic iteration with writing log every @n@ steps.
-- Use 'runWriterT' to exectute it.
writeEvery :: (Monad m, Monoid w) =>
              Int         -- ^ every @n@ iterations
           -> (a -> m a)  -- ^ monadic iteration to repeat
           -> (a -> w)    -- ^ write action to insert
           -> (a -> WriterT w m a)
writeEvery n step write x = do
    x' <- lift (loopM n step x)
    tell (write x')
    return x'

-- | Interleave monadic iteration with IO action every @n@ steps.
ioEvery :: Int           -- ^ every @n@ iterations
        -> (a -> IO a)    -- ^ monadic action to repeat
        -> (a -> IO ())  -- ^ IO action to insert
        -> (a -> IO a)
ioEvery n step io x = do
  x' <- loopM n step x
  io x'
  return x'

loopM :: (Monad m) => Int -> (a -> m a) -> a -> m a
loopM n step !x
    | n == 0    = return x
    | otherwise = step x >>= loopM (n-1) step

-- | Iterations stop when the condition evaluates as @True@.
data Cond a =
      Iteration Int                  -- ^ becomes @True@ after /n/ iterations
    | MaxFitness (Fitness -> Bool)   -- ^ consider the maximal observed fitness
    | MinFitness (Fitness -> Bool)   -- ^ consider the minimal observed fitness
    | AvgFitness (Fitness -> Bool)   -- ^ consider the average observed fitness
    | FitnessStdev (Fitness -> Bool) -- ^ consider standard deviation of fitness
                                     -- within population; may be used to
                                     -- detect premature convergence
    | Or (Cond a) (Cond a)
    | And (Cond a) (Cond a)

evalCond :: (Cond a) -> Population a -> Bool
evalCond (Iteration n) _  = n <= 0
evalCond (MaxFitness cond) p = cond . maxFitness $ p
evalCond (MinFitness cond) p = cond . minFitness $ p
evalCond (AvgFitness cond) p = cond . avgFitness $ p
evalCond (FitnessStdev cond) p = cond . stdDeviation $ p
evalCond (Or c1 c2) x = evalCond c1 x || evalCond c2 x
evalCond (And c1 c2) x = evalCond c1 x && evalCond c2 x

countdownCond :: Cond a -> Cond a
countdownCond (Iteration n) = Iteration (n-1)
countdownCond (And c1 c2) = And (countdownCond c1) (countdownCond c2)
countdownCond (Or c1 c2) = Or (countdownCond c1) (countdownCond c2)
countdownCond c = c

-- | Evaluate fitness for all genomes in the population.
evalFitness :: FitnessFunction a -> [Genome a] -> Population a
evalFitness fitFun gs =
  let fs = map (`fitFun` gs) gs
  in  zip gs fs

-- | Take a list of parents, run crossovers, and return a list of children.
doCrossovers :: [Genome a] -> CrossoverOp a -> Rand [Genome a]
doCrossovers []      _     = return []
doCrossovers parents xover = do
  (children', parents') <- xover parents
  rest <- doCrossovers parents' xover
  return $ children' ++ rest
