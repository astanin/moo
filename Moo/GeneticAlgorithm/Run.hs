{-# LANGUAGE BangPatterns, ExistentialQuantification #-}
{- |

Helper functions to run genetic algorithms and control iterations.

-}

module Moo.GeneticAlgorithm.Run (
  -- * Running algorithm
    runGA
  , runIO
  , nextGeneration
  , evalFitness
  -- * Iteration control
  , loop, loopWithLog, loopIO
  , Cond(..), LogHook(..), IOHook(..)
) where

import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Selection (sortByFitness)
import Moo.GeneticAlgorithm.Types

import Data.Monoid (Monoid, mempty, mappend)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (liftM, when)

-- | Helper function to run the entire algorithm in the 'Rand' monad.
-- It takes care of generating a new random number generator.
runGA :: FitnessFunction a           -- ^ fitness function
      -> Rand [Genome a]             -- ^ function to create initial population
      -> (Population a -> Rand b)    -- ^ genetic algorithm, see also 'loop' and 'loopWithLog'
      -> IO b                        -- ^ final population
runGA fitness initialize ga = do
  rng <- newPureMT
  let (genomes0, rng') = runRandom initialize rng
  let pop0 = evalFitness fitness genomes0
  return $ evalRandom (ga pop0) rng'

-- | Helper function to run the entire algorithm in the 'IO' monad.
runIO :: FitnessFunction a                    -- ^ fitness function
      -> Rand [Genome a]                      -- ^ function to create initial population
      -> (IORef PureMT -> Population a -> IO (Population a))
                                              -- ^ genetic algorithm, see also 'loopIO'
      -> IO (Population a)                    -- ^ final population
runIO fitness initialize gaIO = do
  rng <- newPureMT
  let (genomes0, rng') = runRandom initialize rng
  let pop0 = evalFitness fitness genomes0
  rngref <- newIORef rng'
  gaIO rngref pop0

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
{-# INLINE loop #-}
loop :: (Monad m)
     => Cond a
     -- ^ termination condition @cond@
     -> (Population a -> m (Population a))
     -- ^ @step@ function to produce the next generation
     -> Population a
     -- ^ initial population
     -> m (Population a)
      -- ^ final population
loop cond step pop0 = go cond pop0
  where
    go cond !x
       | evalCond cond x  = return x
       | otherwise        = step x >>= \pop -> go (updateCond pop cond) pop

-- | GA iteration interleaved with the same-monad logging hooks.
{-# INLINE loopWithLog #-}
loopWithLog :: (Monad m, Monoid w)
     => LogHook a m w
     -- ^ periodic logging action
     -> Cond a
     -- ^ termination condition @cond@
     -> (Population a -> m (Population a))
     -- ^ @step@ function to produce the next generation
     -> Population a
     -- ^ initial population
     -> m (Population a, w)
     -- ^ final population
loopWithLog hook cond step pop0 = go cond 0 mempty pop0
  where
    -- go :: Cond a -> Int -> w -> Population a -> m (Population a, w)
    go cond !i !w !x = do
      let logitem = runHook i x hook
      let w' = mappend w logitem
      if (evalCond cond x)
        then return (x, w')
        else step x >>= \pop -> go (updateCond pop cond) (i+1) w' pop
    runHook !i !x (WriteEvery n write)
        | (rem i n) == 0 = write i x
        | otherwise      = mempty


-- | GA iteration interleaved with IO (for logging or saving the
-- intermediate results); it takes and returns the updated random
-- number generator explicitly.
{-# INLINE loopIO #-}
loopIO
     :: [IOHook a]
     -- ^ input-output actions, special and time-dependent stop conditions
     -> Cond a
     -- ^ termination condition @cond@
     -> (Population a -> Rand (Population a))
     -- ^ @step@ function to produce the next generation
     -> IORef PureMT
     -- ^ reference to random number generator
     -> Population a
     -- ^ initial population @pop0@
     -> IO (Population a)
     -- ^ final population
loopIO hooks cond step rngref pop0 = do
  rng <- readIORef rngref
  start <- realToFrac `liftM` getPOSIXTime
  (pop, rng') <- go start cond 0 rng pop0
  writeIORef rngref rng'
  return pop
  where
    go start cond !i !rng !x = do
      stop <- (any id) `liftM` (mapM (runhook start i x) hooks)
      if (stop || evalCond cond x)
         then return (x, rng)
         else do
           let (x', rng') = runRandom (step x) rng
           let i' = i + 1
           let cond' = updateCond x' cond
           go start cond' i' rng' x'

    -- runhook returns True to terminate the loop
    runhook _ i x (DoEvery n io) = do
             when ((rem i n) == 0) (io i x)
             return False
    runhook _ _ _ (StopWhen iotest)  = iotest
    runhook start _ _ (TimeLimit limit)  = do
             now <- realToFrac `liftM` getPOSIXTime
             return (now >= start + limit)

-- | Logging to run every @n@th iteration starting from 0 (the first parameter).
-- The logging function takes the current generation count and population.
data (Monad m, Monoid w) => LogHook a m w =
    WriteEvery Int (Int -> Population a -> w)

-- | Input-output actions, interactive and time-dependent stop conditions.
data IOHook a
    = DoEvery { io'n :: Int, io'action :: (Int -> Population a -> IO ()) }
    -- ^ action to run every @n@th iteration, starting from 0
    | StopWhen (IO Bool)
    -- ^ custom or interactive stop condition
    | TimeLimit { io't :: Double }
    -- ^ terminate iteration after @t@ seconds

-- | Iterations stop when the condition evaluates as @True@.
data Cond a =
      Generations Int                   -- ^ stop after @n@ generations
    | IfFitness ([Fitness] -> Bool)     -- ^ stop when fitness satisfies the @predicate@
    | forall b . Eq b => GensNoChange
      { c'maxgens ::  Int                 -- ^ max number of generations for an indicator to be the same
      , c'indicator ::  [Fitness] -> b    -- ^ stall indicator function
      , c'counter :: Maybe (b, Int)       -- ^ a counter (initially @Nothing@)
      }                                 -- ^ terminate when evolution stalls
    | Or (Cond a) (Cond a)              -- ^ stop when at least one of two conditions holds
    | And (Cond a) (Cond a)             -- ^ stop when both conditions hold

evalCond :: (Cond a) -> Population a -> Bool
evalCond (Generations n) _  = n <= 0
evalCond (IfFitness cond) p = cond . takeFitnesses $ p
evalCond (GensNoChange n _ Nothing) _ = n <= 1
evalCond (GensNoChange n f (Just (prev, count))) p =
    let new = f . map takeFitness $ p
    in  (new == prev) && (count + 1 > n)
evalCond (Or c1 c2) x = evalCond c1 x || evalCond c2 x
evalCond (And c1 c2) x = evalCond c1 x && evalCond c2 x

updateCond :: Population a -> Cond a -> Cond a
updateCond _ (Generations n) = Generations (n-1)
updateCond p (GensNoChange n f Nothing) =
    GensNoChange n f (Just (f (takeFitnesses p), 1)) -- called 1st time _after_ the 1st iteration
updateCond p (GensNoChange n f (Just (v, c))) =
    let v' = f (takeFitnesses p) in if v' == v
       then GensNoChange n f (Just (v, c+1))
       else GensNoChange n f (Just (v', 1))
updateCond p (And c1 c2) = And (updateCond p c1) (updateCond p c2)
updateCond p (Or c1 c2) = Or (updateCond p c1) (updateCond p c2)
updateCond _ c = c

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

takeFitnesses :: Population a -> [Fitness]
takeFitnesses = map takeFitness
