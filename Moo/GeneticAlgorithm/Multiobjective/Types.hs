{-# LANGUAGE MultiParamTypeClasses, Rank2Types, GADTs, FlexibleInstances #-}

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


instance a1 ~ a2 => GenomeState (MultiPhenotype a1) a2 where
    takeGenome = fst


takeObjectiveValues :: MultiPhenotype a -> [Objective]
takeObjectiveValues = snd


-- | Calculate multiple objective per every genome in the population.
evalAllObjectives
    :: forall fn gt a . (ObjectiveFunction fn a, GenomeState gt a)
    => MultiObjectiveProblem fn    -- ^ a list of @problems@
    -> [gt]                        -- ^ a population of @genomes@
    -> [MultiPhenotype a]
evalAllObjectives problems genomes =
    let rawgenomes = map takeGenome genomes
        pops_per_objective = map (\(_, f) -> evalObjective f rawgenomes) problems
        ovs_per_objective = map (map takeObjectiveValue) pops_per_objective
        ovs_per_genome = transpose ovs_per_objective
    in  zip rawgenomes ovs_per_genome
