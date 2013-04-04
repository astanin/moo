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


import Control.Monad (forM_)
import Data.Array (Array, array, (!), elems)
import Data.Array.ST (runSTArray, newArray, readArray, writeArray)
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
        -- FIXME: this is probably O(m N^3 log N), replace with the imperative fast non-dominated sort
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


-- | Crowding distance of a point @p@, as defined by Deb et
-- al. (2002), is an estimate (the sum of dimensions in their
-- pseudocode) of the largest cuboid enclosing the point without
-- including any other point in the population.
crowdingDistances :: [[Objective]] -> [Double]
crowdingDistances [] = []
crowdingDistances pop@(objvals:_) =
    let m = length objvals  -- number of objectives
        n = length pop      -- number of genomes
        inf = 1.0/0.0 :: Double
        sortByObjective i = sortIndicesBy (compare `on` (!! i)) pop
        -- (genome-idx, objective-idx) -> objective value
        ovTable = array ((0,0), (n-1, m-1))
                  [ ((i, objid), (pop !! i) !! objid)
                  | i <- [0..(n-1)], objid <- [0..(m-1)] ]
        -- calculate crowding distances
        distances = runSTArray $ do
          ss <- newArray (0, n-1) 0.0  -- initialize distances
          forM_ [0..(m-1)] $ \objid -> do    -- for every objective
            let ixs = sortByObjective objid
              -- for all inner points
            forM_ (zip3 ixs (drop 1 ixs) (drop 2 ixs)) $ \(iprev, i, inext) -> do
              sum_of_si <- readArray ss i
              let si = (ovTable ! (inext, objid)) - (ovTable ! (iprev, objid))
              writeArray ss i (sum_of_si + si)
            writeArray ss (head ixs) inf   -- boundary points have infinite cuboids
            writeArray ss (last ixs) inf
          return ss
    in elems distances


sortIndicesBy :: (a -> a -> Ordering) -> [a] -> [Int]
sortIndicesBy cmp xs = map snd $ sortBy (cmp `on` fst) (zip xs (iterate (+1) 0))