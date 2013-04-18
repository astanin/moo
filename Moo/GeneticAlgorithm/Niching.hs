module Moo.GeneticAlgorithm.Niching
    ( fitnessSharing
    ) where


import Moo.GeneticAlgorithm.Types


-- | A popular niching method proposed by D. Goldberg and
-- J. Richardson in 1987. The shared fitness of the individual is inversely
-- protoptional to its niche count.
-- The method expects the objective function to be non-negative.
--
-- An extension for minimization problems is implemented by
-- making the fitnes proportional to its niche count (rather than
-- inversely proportional).
--
-- Reference: Chen, J. H., Goldberg, D. E., Ho, S. Y., & Sastry,
-- K. (2002, July). Fitness inheritance in multiobjective
-- optimization. In Proceedings of the Genetic and Evolutionary
-- Computation Conference (pp. 319-326). Morgan Kaufmann Publishers
-- Inc..
fitnessSharing ::
    (Phenotype a -> Phenotype a -> Double)  -- ^ distance function
    -> Double                        -- ^ niche radius
    -> Double                        -- ^ niche compression exponent @alpha@ (usually 1)
    -> ProblemType                   -- ^ type of the optimization problem
    -> Population a
    -> Population a
fitnessSharing dist r alpha Maximizing phenotypes =
    let ms = map (nicheCount dist r alpha phenotypes) phenotypes
    in  zipWith (\(genome, value) m -> (genome, value/m)) phenotypes ms
fitnessSharing dist r alpha Minimizing phenotypes =
    let ms = map (nicheCount dist r alpha phenotypes) phenotypes
    in  zipWith (\(genome, value) m -> (genome, value*m)) phenotypes ms


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
