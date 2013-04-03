{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, GADTs #-}

module Moo.GeneticAlgorithm.Types
    (
    -- * Data structures
      Genome
    , Objective
    , Phenotype
    , Population
    , takeGenome
    , takeObjectiveValue
    -- * GA operators
    , ObjectiveFunction(..)
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

-- | A function to evaluate a genome should be an instance of
-- 'ObjectiveFunction' class. It may be called a cost function for
-- minimization problems, or a fitness function for maximization
-- problems.
--
-- Some genetic algorithm operators, like 'rouletteSelect', require
-- the 'Objective' to be non-negative.
class ObjectiveFunction f a where
    evalObjective :: f -> [Genome a] -> Population a

-- | Evaluate fitness (cost) values genome per genome.
instance (a1 ~ a2) =>
    ObjectiveFunction (Genome a1 -> Objective) a2 where
        evalObjective f = map (\g -> (g, f g))

-- | Evaluate all fitness (cost) values at once.
instance (a1 ~ a2) =>
    ObjectiveFunction ([Genome a1] -> [Objective]) a2 where
        evalObjective f gs = zip gs (f gs)

-- | Evaluate fitness (cost) genome by genome taking all other genomes
-- in consideration too.
instance (a1 ~ a2) =>
    ObjectiveFunction (Genome a1 -> [Genome a1] -> Objective) a2 where
        evalObjective = evalObjectiveBackCompat

{-# DEPRECATED evalObjectiveBackCompat "old ObjectiveFunction type for BC" #-}
evalObjectiveBackCompat :: (Genome a -> [Genome a] -> Objective) -> [Genome a] -> Population a
evalObjectiveBackCompat f gs = map (\g -> (g, f g gs)) gs


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
