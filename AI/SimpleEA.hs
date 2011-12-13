{-# Language BangPatterns #-}

{- |
Copyright    : 2010-2011 Erlend Hamberg, 2011 Sergey Astanin
License      : BSD3
Stability    : experimental
Portability  : portable

A framework for simple genetic algorithms.

Genetic algorithms are used to find good solutions to optimization
and search problems. They mimic the process of natural evolution
and selection.

A genetic algorithm deals with a “/population/” of candidate
solutions.  Each candidate solution is represented with a
“/genome/”. On every iteration the most fittest genomes are
/selected/. The next generation is produced through /crossover/
(recombination of the parents) and /mutation/ (a random change in the
genome) of the selected genomes. This process of selection --
crossover -- mutation is repeated until a good enough solution appears
or all hope is lost.

Sometimes genetic algorithms are defined in terms of minimizing
a cost function rather than maximizing fitness.

/How to write a Genetic Algorithm/

  1. Provide an encoding and decoding functions to convert from model
     variables to “genomes” and back. See /Choosing an encoding/ below.

  2. Write a custom fitness function to maximize. Its type should be
     'FitnessFunction' @a@.

  3. Optionally write custom selection ('SelectionOp'), crossover
     ('CrossoverOp') and mutation ('MutationOp') operators or just use
     some standard operators provided by this library, see
     "AI.SimpleEA.Utils".

  4. Generate an initial population randomly, encode it to genomes,
     evaluate fitness ('evalFitness'). "AI.SimpleEA.Rand" provides
     necessary functions to generate random variables.

  5. Use 'nextGeneration' and 'loopUntil' or 'loopUntil'' to repeat
     the iterations as long as necessary.

Library functions which need access to random number generator work in
'Rand' monad. Wrap such functions with 'runGA' or 'runRandom'.

/Choosing an encoding/

 * For discrete variables the binary (or Gray) encoding may be the
   most suitable. In this library it is represented as a list of
   @Bool@ values (@[Bool]@). Gray code has an advantage, because it
   doesn't suffer from Humming cliff, when two successive variable
   values may be represented by very different
   genomes. "AI.SimpleEA.Utils#encode" provides some tools to encode
   and decode binary genomes. To encode more than one variable, just
   concatenate their codes.

 * For continuous (real-valued) problem variables the user may either
   discretize them or make a genome directly of real-valued variables.
   The former case is equivalent to discrete variables and allows for
   binary/Gray coding and usual crossover and mutation operators, but
   suffers from fixed precision. 'encodeGrayReal' and 'decodeGrayReal'
   from "AI.SimpleEA.Utils#encode" implement such a discretization. In
   the latter case (@[Double]@ or @[Float]@ as a genome) special
   crossover and mutation operators should be used.

/Other modules/

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
import AI.SimpleEA.Utils (minFitness, maxFitness, avgFitness, stdDeviation)
import AI.SimpleEA.Utils (sortByFitness)
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
-- Selection: 'rouletteSelect', 'tournamentSelect'.
--
-- Crossover: 'onePointCrossover', 'twoPointCrossover', 'uniformCrossover',
-- 'simulatedBinaryCrossover'.
--
-- Mutation: 'pointMutate', 'gaussianMutate'.
nextGeneration ::
    Int ->                -- ^ @elite@, the number of genomes to keep intact
    FitnessFunction a ->  -- ^ fitness function
    SelectionOp a ->      -- ^ selection operator
    CrossoverOp a ->      -- ^ crossover operator
    MutationOp a ->       -- ^ mutation operator
    Population a ->       -- ^ current population
    Rand (Population a)  -- ^ next generation
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
    | AvgFitness (Fitness -> Bool)    -- ^ consider the average observed fitness
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
evalCond (AvgFitness cond) p = cond . avgFitness $ p
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

-- | Take a list of parents, run crossovers, and return a list of children.
doCrossovers :: [Genome a] -> CrossoverOp a -> Rand [Genome a]
doCrossovers []      _     = return []
doCrossovers parents xover = do
  (children', parents') <- xover parents
  rest <- doCrossovers parents' xover
  return $ children' ++ rest

{- $SimpleEAExample

TO BE WRITTEN. See @examples/@ folder of the source distribution.

-}
