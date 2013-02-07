module Moo.GeneticAlgorithm.Types
    (
    -- * Data structures
      Fitness
    , Genome
    , Population
    , takeGenome, takeFitness
    -- * GA operators
    , FitnessFunction
    , SelectionOp
    , CrossoverOp
    , MutationOp
    -- * Dummy operators
    , noMutation
    , noCrossover
    ) where

import Moo.GeneticAlgorithm.Random

type Fitness = Double
type Genome a = [a]
type Population a = [(Genome a, Fitness)]

takeGenome :: (Genome a, Fitness) -> Genome a
takeGenome = fst

takeFitness :: (Genome a, Fitness) -> Fitness
takeFitness = snd

-- | A fitness functions assigns a fitness score to a genome. The rest of the
-- individuals of that generation is also provided in case the fitness is
-- in proportion to its neighbours. Genetic algorithm maximizes the fitness.
--
-- Some genetic algorithm operators, like 'rouletteSelect', require
-- non-negative fitness.  To solve minimization problems consider
-- transforming the fitness value as
--
-- @
-- F(x) = 1/(1+f(x))
-- @
type FitnessFunction a = Genome a -> [Genome a] -> Fitness

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
