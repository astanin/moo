module Moo.GeneticAlgorithm.Niching
    ( withFitnessSharing
    ) where


import Moo.GeneticAlgorithm.Types


-- | A popular niching method proposed by D. Goldberg and
-- J. Richardson in 1987. The shared fitness of the individual is inversely
-- protoptional to its niche count.
--
-- The method requires the objective function to be non-negative.
-- An extension for minimization problems is implemented by
-- making the fitnes proportional to its niche count.
--
-- Reference: Chen, J. H., Goldberg, D. E., Ho, S. Y., & Sastry,
-- K. (2002, July). Fitness inheritance in multiobjective
-- optimization. In Proceedings of the Genetic and Evolutionary
-- Computation Conference (pp. 319-326). Morgan Kaufmann Publishers
-- Inc..
withFitnessSharing :: ObjectiveFunction fn a
    => (Phenotype a -> Phenotype a -> Double)  -- ^ distance function
    -> Double                        -- ^ niche radius
    -> Double                        -- ^ niche compression exponent @alpha@ (usually 1)
    -> ProblemType                   -- ^ type of the optimization problem
    -> fn                            -- ^ an objective function without niching
    -> ([Genome a] -> [Objective])   -- ^ a new objective function with fitness sharing
withFitnessSharing dist r alpha Maximizing objective = \genomes ->
    let phenotypes = evalObjective objective genomes
        ms = map (nicheCount dist r alpha phenotypes) phenotypes
    in  zipWith (\x m -> (takeObjectiveValue x)/m) phenotypes ms
withFitnessSharing dist r alpha Minimizing objective = \genomes ->
    let phenotypes = evalObjective objective genomes
        ms = map (nicheCount dist r alpha phenotypes) phenotypes
    in  zipWith (\x m -> (takeObjectiveValue x)*m) phenotypes ms


type DistanceFunction a = Phenotype a -> Phenotype a -> Double


nicheCount :: DistanceFunction a
           -> Double -> Double
           -> Population a -> Phenotype a -> Double
nicheCount dist r alpha population phenotype =
    sum $ map (sharing dist r alpha phenotype) population


sharing :: DistanceFunction a
        -> Double -> Double
        -> DistanceFunction a
sharing dist r alpha pi pj =
    let dij = dist pi pj
    in  if dij < r
        then 1.0 - (dij/r)**alpha
        else 0.0
