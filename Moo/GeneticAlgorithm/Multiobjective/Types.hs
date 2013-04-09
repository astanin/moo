{-# LANGUAGE Rank2Types #-}

module Moo.GeneticAlgorithm.Multiobjective.Types
    ( SingleObjectiveProblem
    , MultiObjectiveProblem
    , MultiPhenotype
    , evalAllObjectives
    , takeObjectiveValues
    ) where


import Moo.GeneticAlgorithm.Types


import Data.List (transpose)


type SingleObjectiveProblem fn = ( ProblemType , fn )
type MultiObjectiveProblem fn = [SingleObjectiveProblem fn]


-- | An individual with all objective functions evaluated.
type MultiPhenotype a = (Genome a, [Objective])


takeObjectiveValues :: MultiPhenotype a -> [Objective]
takeObjectiveValues = snd


-- | Calculate multiple objective per every genome in the population.
evalAllObjectives
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn    -- ^ a list of @problems@
    -> [Genome a]                  -- ^ a population of raw @genomes@
    -> [MultiPhenotype a]
evalAllObjectives problems genomes =
    let pops_per_objective = map (\(_, f) -> evalObjective f genomes) problems
        ovs_per_objective = map (map takeObjectiveValue) pops_per_objective
        ovs_per_genome = transpose ovs_per_objective
    in  zip genomes ovs_per_genome
