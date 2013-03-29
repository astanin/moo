module Moo.GeneticAlgorithm.Types
    (
    -- * Data structures
      Genome
    , Objective
    , Phenotype
    , Population
    , takeGenome, takeObjectiveValue
    -- * GA operators
    , ObjectiveFunction
    , SelectionOp
    , CrossoverOp
    , MutationOp
    -- * Dummy operators
    , noMutation
    , noCrossover
    ) where

import Moo.GeneticAlgorithm.Random

-- | A genetic representation of an individual solution.
type Genome a = [a]

-- | A measure of the observed performance. It may be called cost
-- for minimization problems, or fitness for maximization problems.
type Objective = Double

-- | A genome associated with its observed 'Objective' value.
type Phenotype a = (Genome a, Objective)

-- | An entire population of observed 'Phenotype's.
type Population a = [Phenotype a]

takeGenome :: Phenotype a -> Genome a
takeGenome = fst

takeObjectiveValue :: Phenotype a -> Objective
takeObjectiveValue = snd

-- | A function to evaluate a genome. It may be called a cost function
-- for minimization problems, or a fitness function for maximization
-- problems.
--
-- Some genetic algorithm operators, like 'rouletteSelect', require
-- the 'Objective' to be non-negative.
type ObjectiveFunction a = Genome a -> [Genome a] -> Objective

-- | A selection operator is responsible for selection. It takes pairs of
-- genomes and their fitness and is responsible for returning one or more
-- individuals.
type SelectionOp a = Population a -> Rand [Genome a]

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
