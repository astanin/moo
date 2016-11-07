{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs, ExistentialQuantification #-}

module Moo.GeneticAlgorithm.Types
    (
    -- * Data structures
      Genome
    , Objective
    , Phenotype
    , Population
    , GenomeState(..)
    , takeObjectiveValue
    -- * GA operators
    , ProblemType (..)
    , ObjectiveFunction(..)
    , SelectionOp
    , CrossoverOp
    , MutationOp
    -- * Dummy operators
    , noMutation
    , noCrossover
    -- * Life cycle
    , StepGA
    , Cond(..)
    , PopulationState
    , StepResult(..)
    ) where

import Moo.GeneticAlgorithm.Random
import Control.Parallel.Strategies (parMap, rseq)

-- | A genetic representation of an individual solution.
type Genome a = [a]

-- | A measure of the observed performance. It may be called cost
-- for minimization problems, or fitness for maximization problems.
type Objective = Double

-- | A genome associated with its observed 'Objective' value.
type Phenotype a = (Genome a, Objective)

-- | An entire population of observed 'Phenotype's.
type Population a = [Phenotype a]


-- | 'takeGenome' extracts a raw genome from any type which embeds it.
class GenomeState gt a where
    takeGenome :: gt -> Genome a


instance (a1 ~ a2) => GenomeState (Genome a1) a2 where
    takeGenome = id


instance (a1 ~ a2) => GenomeState (Phenotype a1) a2 where
    takeGenome = fst


takeObjectiveValue :: Phenotype a -> Objective
takeObjectiveValue = snd

-- | A type of optimization problem: whether the objective function
-- has to be miminized, or maximized.
data ProblemType = Minimizing | Maximizing deriving (Show, Eq)

-- | A function to evaluate a genome should be an instance of
-- 'ObjectiveFunction' class. It may be called a cost function for
-- minimization problems, or a fitness function for maximization
-- problems.
--
-- Some genetic algorithm operators, like 'rouletteSelect', require
-- the 'Objective' to be non-negative.
class ObjectiveFunction f a where
    evalObjective :: f -> [Genome a] -> Population a

-- | Evaluate fitness (cost) values genome per genome in parallel.
instance (a1 ~ a2) =>
    ObjectiveFunction (Genome a1 -> Objective) a2 where
        evalObjective f gs = parMap rseq (\g -> (g, f g)) gs

-- | Evaluate all fitness (cost) values at once.
instance (a1 ~ a2) =>
    ObjectiveFunction ([Genome a1] -> [Objective]) a2 where
        evalObjective f gs = zip gs (f gs)

-- | Evaluate fitness (cost) of all genomes, possibly changing their
-- order.
instance (a1 ~ a2) =>
    ObjectiveFunction ([Genome a1] -> [(Genome a1, Objective)]) a2 where
        evalObjective f gs = f gs

-- | A selection operator selects a subset (probably with repetition)
-- of genomes for reproduction via crossover and mutation.
type SelectionOp a = Population a -> Rand (Population a)

-- | A crossover operator takes some /parent/ genomes and returns some
-- /children/ along with the remaining parents. Many crossover
-- operators use only two parents, but some require three (like UNDX)
-- or more. Crossover operator should consume as many parents as
-- necessary and stop when the list of parents is empty.
type CrossoverOp a = [Genome a] -> Rand ([Genome a], [Genome a])

-- | A mutation operator takes a genome and returns an altered copy of it.
type MutationOp a = Genome a -> Rand (Genome a)

-- | Don't crossover.
noCrossover :: CrossoverOp a
noCrossover genomes = return (genomes, [])

-- | Don't mutate.
noMutation :: MutationOp a
noMutation = return


-- | A single step of the genetic algorithm. See also 'nextGeneration'.
type StepGA m a = Cond a              -- ^ stop condition
                -> PopulationState a  -- ^ population of the current generation
                -> m (StepResult (Population a))  -- ^ population of the next generation


-- | Iterations stop when the condition evaluates as @True@.
data Cond a =
      Generations Int                   -- ^ stop after @n@ generations
    | IfObjective ([Objective] -> Bool) -- ^ stop when objective values satisfy the @predicate@
    | forall b . Eq b => GensNoChange
      { c'maxgens ::  Int                 -- ^ max number of generations for an indicator to be the same
      , c'indicator ::  [Objective] -> b  -- ^ stall indicator function
      , c'counter :: Maybe (b, Int)       -- ^ a counter (initially @Nothing@)
      }                                 -- ^ terminate when evolution stalls
    | Or (Cond a) (Cond a)              -- ^ stop when at least one of two conditions holds
    | And (Cond a) (Cond a)             -- ^ stop when both conditions hold


{-| On life cycle of the genetic algorithm:

>
>   [ start ]
>       |
>       v
>   (genomes) --> [calculate objective] --> (evaluated genomes) --> [ stop ]
>       ^  ^                                       |
>       |  |                                       |
>       |  `-----------.                           |
>       |               \                          v
>   [ mutate ]        (elite) <-------------- [ select ]
>       ^                                          |
>       |                                          |
>       |                                          |
>       |                                          v
>   (genomes) <----- [ crossover ] <-------- (evaluted genomes)
>

PopulationState can represent either @genomes@ or @evaluated genomed@.
-}
type PopulationState a = Either [Genome a] [Phenotype a]


-- | A data type to distinguish the last and intermediate steps results.
data StepResult a = StopGA a | ContinueGA a deriving (Show)
