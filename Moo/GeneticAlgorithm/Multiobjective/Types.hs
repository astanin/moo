{-# LANGUAGE Rank2Types #-}

module Moo.GeneticAlgorithm.Multiobjective.Types
    ( SingleObjectiveProblem
    , MultiObjectiveProblem
    , EvaluatedGenome
    , evalAllObjectives
    ) where


import Moo.GeneticAlgorithm.Types


import Data.List (transpose)


type SingleObjectiveProblem fn = ( ProblemType , fn )
type MultiObjectiveProblem fn = [SingleObjectiveProblem fn]


-- | A solution with all objective functions evaluated.
type EvaluatedGenome a = (Genome a, [Objective])


-- | Calculate multiple objective per every genome in the population.
evalAllObjectives
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn    -- ^ a list of @problems@
    -> [Genome a]                  -- ^ a population of raw @genomes@
    -> [EvaluatedGenome a]
evalAllObjectives problems genomes =
    let pops_per_objective = map (\(_, f) -> evalObjective f genomes) problems
        ovs_per_objective = map (map takeObjectiveValue) pops_per_objective
        ovs_per_genome = transpose ovs_per_objective
    in  zip genomes ovs_per_genome
