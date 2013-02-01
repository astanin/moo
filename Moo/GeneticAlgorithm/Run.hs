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
  , loopUntilWithHooks
  , loopUntilWithIO
  , Cond(..), WriterHook(..)
) where

import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Utilities (minFitness, maxFitness, avgFitness, stdDeviation)
import Moo.GeneticAlgorithm.Selection (sortByFitness)
import Moo.GeneticAlgorithm.Types

import Data.Monoid (Monoid, mempty, mconcat)

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
loopUntil :: (Monad m) =>
             Cond a
          -- ^ termination condition @cond@
          -> (Population a -> m (Population a))
          -- ^ @step@ function to produce the next generation
          -> Population a
          -- ^ initial population
          -> m (Population a)
           -- ^ final population
loopUntil cond step pop0 = go cond pop0
  where
    go cond !x
       | evalCond cond x  = return x
       | otherwise        = step x >>= go (countdownCond cond)

-- | GA iteration interleaved with the same-monad logging hooks.
{-# INLINE loopUntilWithHooks #-}
loopUntilWithHooks :: (Monad m, Monoid w) =>
                      [WriterHook a m w]
                   -- ^ periodic side-effect actions, usually logging
                   -> Cond a
                   -- ^ termination condition @cond@
                   -> (Population a -> m (Population a))
                   -- ^ @step@ function to produce the next generation
                   -> Population a
                   -- ^ initial population
                   -> m (Population a, w)
                   -- ^ final population
loopUntilWithHooks hooks cond step pop0 = go cond 0 mempty pop0
  where
    -- go :: Cond a -> Int -> w -> Population a -> m (Population a, w)
    go cond !i !w !x = do
      ws <- mapM (runHook i x) hooks
      let w' = mconcat (w:ws)
      if (evalCond cond x)
        then return (x, w')
        else step x >>= go (countdownCond cond) (i+1) w'
    runHook !i !x (WriteEvery n write)
        | (rem i n) == 0 = return (write i x)
        | otherwise      = return mempty


-- | GA iteration interleaved with IO (for logging or saving the
-- intermediate results); it takes and returns the updated random
-- number generator explicitly.
{-# INLINE loopUntilWithIO #-}
loopUntilWithIO :: (Int -> Population a -> IO ())
                -- ^ an IO action which takes generation count and the current population;
                -- it is executed for every new generation.
                -> Cond a
                -- ^ termination condition @cond@
                -> (Population a -> Rand (Population a))
                -- ^ @step@ function to produce the next generation
                -> Population a
                -- ^ initial population @pop0@
                -> PureMT
                -- ^ random number generator
                -> IO (Population a, PureMT)
                -- ^ final population and the new state of the random number generator
loopUntilWithIO io cond step pop0 rng = go cond 0 rng pop0
  where
    -- go :: Cond a -> Int -> PureMT -> Population a -> IO (Population a, PureMT)
    go cond !i !rng !x
       | evalCond cond x  = return (x, rng)
       | otherwise        = do
          let (x', rng') = runRandom (step x) rng
          let i' = i + 1
          io i' x'
          go (countdownCond cond) i' rng' x'

-- | Hooks to run every nth iteration starting from 0.
-- The second argument is a function which takes generation count
-- and the current population, and returns some data to save.
data (Monad m, Monoid w) =>
    WriterHook a m w = WriteEvery Int (Int -> Population a -> w)

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
