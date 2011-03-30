module AI.SimpleEA.Types
    (
    -- * Data structures
      Fitness
    , Genome
    , Population
    -- * GA operators
    , FitnessFunction
    , SelectionOp
    , CrossoverOp
    , MutationOp ) where

import AI.SimpleEA.Rand

type Fitness = Double
type Genome a = [a]
type Population a = [(Genome a, Fitness)]

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
--
-- or
--
-- @
-- F(x) = LargePositive - objective(x)/mean(objective(x_i)).
-- @
type FitnessFunction a = Genome a -> [Genome a] -> Fitness

-- | A selection operator is responsible for selection. It takes pairs of
-- genomes and their fitness and is responsible for returning one or more
-- individuals.
type SelectionOp a = Population a -> Rand [Genome a]

-- | A crossover operator takes two /parent/ genomes and returns two
-- /children/.
type CrossoverOp a = (Genome a, Genome a) -> Rand (Genome a, Genome a)

-- | A mutation operator takes a genome and returns an altered copy of it.
type MutationOp a = Genome a -> Rand (Genome a)

