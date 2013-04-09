{-# LANGUAGE Rank2Types #-}

module Moo.GeneticAlgorithm.Multiobjective
    (
    -- * Types
      SingleObjectiveProblem
    , MultiObjectiveProblem
    -- * Evaluation
    , toObjectiveSpace
    , toSolutionSpace
    -- * NSGA-II: A non-dominated sorting genetic algorithm
    , stepNSGA2
    , stepNSGA2default
    ) where

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Multiobjective.Types
import Moo.GeneticAlgorithm.Multiobjective.NSGA2


-- | Calculate multiple objective values per every genome.
toObjectiveSpace
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn      -- ^ a multi-objective problem
    -> [(Genome a, Objective)]       -- ^ ranked genomes (e.g. output of 'stepNSGA')
    -> [[Objective]]
toObjectiveSpace mop rankedgenomes =
    let phenotypes = evalAllObjectives mop (map fst rankedgenomes)
    in  map takeObjectiveValues phenotypes


-- | Extract selected solutions from the output of a multi-objective algorithm.
toSolutionSpace
   :: [(Genome a, Objective)]        -- ^ ranked genomes (e.g. output of 'stepNSGA')
   -> [Genome a]
toSolutionSpace = map fst