{- |

Multiobjective optimization.

-}

module Moo.GeneticAlgorithm.Multiobjective where

{---- NSGA-II ----

Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. A. M. T. (2002). A
fast and elitist multiobjective genetic algorithm:
NSGA-II. Evolutionary Computation, IEEE Transactions on, 6(2),
182-197.

-}


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Selection


import Data.Function (on)
import Data.List (sortBy, groupBy)


-- | A solution @p@ dominates another solution @q@ if at least one 'Objective'
-- values of @p@ is better than the respective value of @q@, and the other
-- are not worse.
dominates :: [ProblemType] -- ^ problem types per every objective
          -> [Objective]   -- ^ 'Objective' values of the solution @p@
          -> [Objective]   -- ^ 'Objective' values of the solution @q@
          -> Bool
dominates ptypes p q =
    let pqs = zip3 ptypes p q
        qps = zip3 ptypes q p
    in  (any better1 pqs) && (all (not . better1) qps)
  where
    better1 :: (ProblemType, Objective, Objective) -> Bool
    better1 (Minimizing, pv, qv) = pv < qv
    better1 (Maximizing, pv, qv) = pv > qv


type EvaluatedGenome a = (Genome a, [Objective])
data DomRank a = DomRank { dr'dominatedBy :: Int
                         , dr'dominates :: [EvaluatedGenome a] }
    deriving (Show, Eq)


-- | Build a list of non-dominated fronts. The best solutions are in the first front.
nondominatedSort :: [ProblemType] -> [EvaluatedGenome a] -> [[EvaluatedGenome a]]
nondominatedSort ptypes gs =
    let drs = map (dr'dominatedBy . genomeDomRank ptypes gs) gs
        -- FIXME: this is probably O(m N^3 log N), replace with imperative fast non-dominated sort
        fronts = groupBy ((==) `on` snd) . sortBy (compare `on` snd) $ zip gs drs
    in  map (map fst) fronts


-- | Calculate the number of solutions which dominate the genome,
-- and build a set of solutions which the genome dominates
genomeDomRank :: [ProblemType] -> [EvaluatedGenome a] -> EvaluatedGenome a -> DomRank a
genomeDomRank ptypes allGenomes genome =
    let this = snd genome
        -- dominating are better than this genome
        dominating = filter (\(_,other) -> dominates ptypes other this) allGenomes
        -- this genome is better than dominated ones
        dominated  = filter (\(_,other) -> dominates ptypes this other) allGenomes
    in  DomRank (length dominating) dominated