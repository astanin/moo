{-# LANGUAGE BangPatterns, Rank2Types #-}
{-# LANGUAGE GADTs #-}
{- |

Helper functions to run genetic algorithms and control iterations.

-}

module Moo.GeneticAlgorithm.Run (
  -- * Running algorithm
    runGA
  , runIO
  , nextGeneration
  , nextSteadyState
  , makeStoppable
  -- * Iteration control
  , loop, loopWithLog, loopIO
  , Cond(..), LogHook(..), IOHook(..)
) where

import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Selection (bestFirst)
import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.StopCondition
import Moo.GeneticAlgorithm.Utilities (doCrossovers, doNCrossovers)

import Data.Monoid (Monoid, mempty, mappend)
import Data.Time.Clock.POSIX (getPOSIXTime)
import Data.IORef (IORef, newIORef, readIORef, writeIORef)
import Control.Monad (liftM, when)

-- | Helper function to run the entire algorithm in the 'Rand' monad.
-- It takes care of generating a new random number generator.
runGA :: Rand [Genome a]             -- ^ function to create initial population
      -> ([Genome a] -> Rand b)       -- ^ genetic algorithm, see also 'loop' and 'loopWithLog'
      -> IO b                        -- ^ final population
runGA initialize ga = do
  rng <- newPureMT
  let (genomes0, rng') = runRand initialize rng
  return $ evalRand (ga genomes0) rng'

-- | Helper function to run the entire algorithm in the 'IO' monad.
runIO :: Rand [Genome a]                  -- ^ function to create initial population
      -> (IORef PureMT -> [Genome a] -> IO (Population a))
                                          -- ^ genetic algorithm, see also 'loopIO'
      -> IO (Population a)                -- ^ final population
runIO initialize gaIO = do
  rng <- newPureMT
  let (genomes0, rng') = runRand initialize rng
  rngref <- newIORef rng'
  gaIO rngref genomes0

-- | Construct a single step of the genetic algorithm.
--
-- See "Moo.GeneticAlgorithm.Binary" and "Moo.GeneticAlgorithm.Continuous"
-- for the building blocks of the algorithm.
--
nextGeneration
    :: (ObjectiveFunction objectivefn a)
    => ProblemType          -- ^ a type of the optimization @problem@
    -> objectivefn          -- ^ objective function
    -> SelectionOp a        -- ^ selection operator
    -> Int                  -- ^ @elite@, the number of genomes to keep intact
    -> CrossoverOp a        -- ^ crossover operator
    -> MutationOp a         -- ^ mutation operator
    -> StepGA Rand a
nextGeneration problem objective selectOp elite xoverOp mutationOp =
  makeStoppable objective $ \pop -> do
    genomes' <- liftM (map takeGenome) $ withElite problem elite selectOp pop
    let top = take elite genomes'
    let rest = drop elite genomes'
    genomes' <- shuffle rest         -- just in case if @selectOp@ preserves order
    genomes' <- doCrossovers genomes' xoverOp
    genomes' <- mapM mutationOp genomes'
    return $ evalObjective objective (top ++ genomes')


-- | Construct a single step of the incremental (steady-steate) genetic algorithm.
-- Exactly @n@ worst solutions are replaced with newly born children.
--
-- See "Moo.GeneticAlgorithm.Binary" and "Moo.GeneticAlgorithm.Continuous"
-- for the building blocks of the algorithm.
--
nextSteadyState
    :: (ObjectiveFunction objectivefn a)
    => Int                  -- ^ @n@, number of worst solutions to replace
    -> ProblemType          -- ^ a type of the optimization @problem@
    -> objectivefn          -- ^ objective function
    -> SelectionOp a        -- ^ selection operator
    -> CrossoverOp a        -- ^ crossover operator
    -> MutationOp a         -- ^ mutation operator
    -> StepGA Rand a
nextSteadyState n problem objective selectOp crossoverOp mutationOp =
    makeStoppable objective $ \pop -> do
      let popsize = length pop
      parents <- liftM (map takeGenome) (selectOp pop)
      children <- mapM mutationOp =<< doNCrossovers n parents crossoverOp
      let sortedPop = bestFirst problem pop
      let cpop = evalObjective objective children
      return . take popsize $ cpop ++ sortedPop


-- | Wrap a population transformation with pre- and post-conditions
-- to indicate the end of simulation.
--
-- Use this function to define custom replacement strategies
-- in addition to 'nextGeneration' and 'nextSteadyState'.
makeStoppable
    :: (ObjectiveFunction objectivefn a, Monad m)
    => objectivefn
    -> (Population a -> m (Population a))  -- single step
    -> StepGA m a
makeStoppable objective onestep stop input = do
  let pop = either (evalObjective objective) id input
  if isGenomes input && evalCond stop pop
     then return $ StopGA pop   -- stop before the first iteration
     else do
       newpop <- onestep pop
       return $ if evalCond stop newpop
                then StopGA newpop
                else ContinueGA newpop
  where
    isGenomes (Left _) = True
    isGenomes (Right _) = False


-- | Select @n@ best genomes, then select more genomes from the
-- /entire/ population (elite genomes inclusive). Elite genomes will
-- be the first in the list.
withElite :: ProblemType -> Int -> SelectionOp a -> SelectionOp a
withElite problem n select = \population -> do
  let elite = take n . eliteGenomes $ population
  selected <- select population
  return (elite ++ selected)
  where
    eliteGenomes = bestFirst problem

-- | Run strict iterations of the genetic algorithm defined by @step@.
-- Return the result of the last step.  Usually only the first two
-- arguments are given, and the result is passed to 'runGA'.
{-# INLINE loop #-}
loop :: (Monad m)
     => Cond a
     -- ^ termination condition @cond@
     -> StepGA m a
     -- ^ @step@ function to produce the next generation
     -> [Genome a]
     -- ^ initial population
     -> m (Population a)
      -- ^ final population
loop cond step genomes0 = go cond (Left genomes0)
  where
    go cond !x = do
       x' <- step cond x
       case x' of
         (StopGA pop) -> return pop
         (ContinueGA pop) -> go (updateCond pop cond) (Right pop)

-- | GA iteration interleaved with the same-monad logging hooks.
-- Usually only the first three arguments are given, and the result is
-- passed to 'runGA'.
{-# INLINE loopWithLog #-}
loopWithLog :: (Monad m, Monoid w)
     => LogHook a m w
     -- ^ periodic logging action
     -> Cond a
     -- ^ termination condition @cond@
     -> StepGA m a
     -- ^ @step@ function to produce the next generation
     -> [Genome a]
     -- ^ initial population
     -> m (Population a, w)
     -- ^ final population
loopWithLog hook cond step genomes0 = go cond 0 mempty (Left genomes0)
  where
    go cond !i !w !x = do
      x' <- step cond x
      case x' of
        (StopGA pop) -> return (pop, w)
        (ContinueGA pop) -> do
                         let w' = mappend w (runHook i pop hook)
                         let cond' = updateCond pop cond
                         go cond' (i+1) w' (Right pop)

    runHook !i !x (WriteEvery n write)
        | (rem i n) == 0 = write i x
        | otherwise      = mempty


-- | GA iteration interleaved with IO (for logging or saving the
-- intermediate results); it takes and returns the updated random
-- number generator via an IORef. Usually only the first three
-- arguments are given, and the result is passed to 'runIO'.
{-# INLINE loopIO #-}
loopIO
     :: [IOHook a]
     -- ^ input-output actions, special and time-dependent stop conditions
     -> Cond a
     -- ^ termination condition @cond@
     -> StepGA Rand a
     -- ^ @step@ function to produce the next generation
     -> IORef PureMT
     -- ^ reference to the random number generator
     -> [Genome a]
     -- ^ initial population @pop0@
     -> IO (Population a)
     -- ^ final population
loopIO hooks cond step rngref genomes0 = do
  rng <- readIORef rngref
  start <- realToFrac `liftM` getPOSIXTime
  (pop, rng') <- go start cond 0 rng (Left genomes0)
  writeIORef rngref rng'
  return pop
  where
    go start cond !i !rng !x = do
      stop <- (any id) `liftM` (mapM (runhook start i x) hooks)
      if (stop || either (const False) (evalCond cond) x)
         then return (asPopulation x, rng)
         else do
           let (x', rng') = runRand (step cond x) rng
           case x' of
             (StopGA pop) -> return (pop, rng')
             (ContinueGA pop) ->
                 do
                   let i' = i + 1
                   let cond' = updateCond pop cond
                   go start cond' i' rng' (Right pop)

    -- runhook returns True to terminate the loop
    runhook _ i x (DoEvery n io) = do
             when ((rem i n) == 0) (io i (asPopulation x))
             return False
    runhook _ _ _ (StopWhen iotest)  = iotest
    runhook start _ _ (TimeLimit limit)  = do
             now <- realToFrac `liftM` getPOSIXTime
             return (now >= start + limit)

    -- assign dummy objective value to a genome
    dummyObjective :: Genome a -> Phenotype a
    dummyObjective g = (g, 0.0)

    asPopulation = either (map dummyObjective) id

-- | Logging to run every @n@th iteration starting from 0 (the first parameter).
-- The logging function takes the current generation count and population.
data LogHook a m w where
    WriteEvery :: (Monad m, Monoid w)
               => Int
               -> (Int -> Population a -> w)
               -> LogHook a m w

-- | Input-output actions, interactive and time-dependent stop conditions.
data IOHook a
    = DoEvery { io'n :: Int, io'action :: (Int -> Population a -> IO ()) }
    -- ^ action to run every @n@th iteration, starting from 0;
    -- initially (at iteration 0) the objective value is zero.
    | StopWhen (IO Bool)
    -- ^ custom or interactive stop condition
    | TimeLimit { io't :: Double }
    -- ^ terminate iteration after @t@ seconds
