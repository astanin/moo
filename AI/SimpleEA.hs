{-# Language BangPatterns #-}

{- |
Copyright    : 2010-2011 Erlend Hamberg, 2011 Sergey Astanin
License      : BSD3
Stability    : experimental
Portability  : portable

A framework for simple genetic algorithms.

"AI.SimpleEA.Utils" module contains utility functions that implement
some most common types of genetic operators: encoding, selection, mutation,
crossover.

"AI.SimpleEA.Rand" module provides some additional facilities to make
'PureMT' random number generator as convenient as the standard
generator.

-}

module AI.SimpleEA (
  -- * Running algorithm
    runGA
  , nextGeneration
  , evalFitness
  , loopUntil, loopUntil'
  , iterateUntil, iterateUntil'
  , Cond(..)
  -- * Types
  , Fitness
  , Genome
  , Population
  , FitnessFunction
  , SelectionOp
  , CrossoverOp
  , MutationOp
  -- * Example
  -- $SimpleEAExample
) where

import AI.SimpleEA.Rand
import AI.SimpleEA.Utils (minFitness, maxFitness, stdDeviation)
import AI.SimpleEA.Types

import Control.Monad (liftM)

-- | Helper function to run an entire algorithm in the 'Rand' monad.
-- It takes care of generating a new random number generator.
runGA :: Rand a -> IO a
runGA ga = do
  rng <- newPureMT
  return $ evalRandom ga rng

-- | A single step of the genetic algorithm.
--
-- "AI.SimpleEA.Utils" provides some operators which may be used as
-- building blocks of the algorithm.
--
-- Search space encoding: 'encodeGray', 'encodeGrayReal' and
-- 'decodeGray', 'decodeGrayReal' respectively.
--
-- Initialization: 'getRandomGenomes'.
--
-- Selection: 'rouletteSelect', 'tournamentSelect'.
--
-- Crossover: 'onePointCrossover', 'twoPointCrossover', 'uniformCrossover'.
--
-- Mutation: 'pointMutate'.
nextGeneration ::
    FitnessFunction a ->
    SelectionOp a ->
    CrossoverOp a ->
    MutationOp a ->
    Population a ->
    Rand (Population a)
nextGeneration fitness selectOp xoverOp mutationOp pop = do
  genomes' <- selectOp pop
  genomes' <- shuffle genomes'  -- just in case if selectOp preserves order
  genomes' <- doCrossovers genomes' xoverOp
  genomes' <- mapM mutationOp genomes'
  return $ evalFitness fitness genomes'

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

-- | Like 'loopUntil' but the user also provides a @digest@ function which
-- extracts some summary from the current generation to keep. 'loopUntil''
-- accumulates these digests and returns the history of population evolution
-- along with the final population.
{-# INLINE loopUntil' #-}
loopUntil' :: (Monad m)
      => Cond a   -- ^ termination condition @cond@
      -> (Population a -> d)  -- ^ @digest@ function to extract summary
      -> Population a  -- ^ initial population
      -> (Population a -> m (Population a))
                      -- ^ @step@ function to produce the next generation
      -> m ((Population a, [d]))  -- ^ final population and its history
loopUntil' cond digest pop0 step =
    second reverse `liftM` go cond pop0 [digest pop0]
  where
    second f (x,y) = (x, f y)
    go cond !x !ds
       | evalCond cond x  = return (x, ds)
       | otherwise        =
           do x' <- step x
              go (countdownCond cond) x' (digest x':ds)

-- | Like 'loopUntil', but argumens' order is more suitable for
-- partial function application.
iterateUntil :: (Monad m) => Cond a -> (Population a -> m (Population a))
             -> Population a -> m (Population a)
iterateUntil cond step pop0 = loopUntil cond pop0 step

-- | Like 'loopUntil'', but argumens' order is more suitable for
-- partial function application.
iterateUntil' :: (Monad m) => Cond a -> (Population a -> d)
             -> (Population a -> m (Population a)) -> Population a
             -> m (Population a, [d])
iterateUntil' cond digest step pop0 = loopUntil' cond digest pop0 step

-- | Iterations stop when the condition evaluates as @True@.
data Cond a =
      Iteration Int                  -- ^ becomes @True@ after /n/ iterations
    | MaxFitness (Fitness -> Bool)    -- ^ consider the maximal observed fitness
    | MinFitness (Fitness -> Bool)    -- ^ consider the minimal observed fitness
    | FitnessStdev (Fitness -> Bool)  -- ^ consider standard deviation of fitness
                                     -- within population; may be used to
                                     -- detect premature convergence
    | EntirePopulation ([Genome a] -> Bool)  -- ^ consider all genomes
    | Or (Cond a) (Cond a)
    | And (Cond a) (Cond a)

evalCond :: (Cond a) -> Population a -> Bool
evalCond (Iteration n) _  = n <= 0
evalCond (MaxFitness cond) p = cond . maxFitness $ p
evalCond (MinFitness cond) p = cond . minFitness $ p
evalCond (FitnessStdev cond) p = cond . stdDeviation $ p
evalCond (EntirePopulation cond) p = cond . map fst $ p
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

doCrossovers :: [Genome a] -> CrossoverOp a -> Rand [Genome a]
doCrossovers []      _   = return []
doCrossovers [_]     _   = error "odd number of parents"
doCrossovers (a:b:r) rec = do
    (a',b') <- rec (a,b)
    rest    <- doCrossovers r rec
    return $ a':b':rest

{- $SimpleEAExample

TO BE WRITTEN

-}
