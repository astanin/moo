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


import Control.Monad (forM_)
import Data.Array (array, (!), elems)
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


-- | A solution with all objective functions evaluated.
type EvaluatedGenome a = (Genome a, [Objective])

data IntermediateRank a = IntermediateRank {
      ir'dominatedBy :: Int
    , ir'dominates :: [EvaluatedGenome a]
    } deriving (Show, Eq)


-- | Solution and its non-dominated rank and local crowding distance.
data RankedSolution a = RankedSolution {
      rs'genome :: EvaluatedGenome a
    , rs'nondominationRank :: Int  -- ^ @0@ is the best
    , rs'localCrowdingDistnace :: Double  -- ^ @Infinity@ for less-crowded boundary points
    } deriving (Show, Eq)


-- | Build a list of non-dominated fronts. The best solutions are in the first front.
-- FIXME: this is probably O(m N^3 log N), replace with the fast imperative non-dominated sort
nondominatedSort :: [ProblemType] -> [EvaluatedGenome a] -> [[EvaluatedGenome a]]
nondominatedSort ptypes gs =
    let drs = map (ir'dominatedBy . rankGenome ptypes gs) gs
        fronts = groupBy ((==) `on` snd) . sortBy (compare `on` snd) $ zip gs drs
    in  map (map fst) fronts


-- | Calculate the number of solutions which dominate the genome,
-- and build a set of solutions which the genome dominates
rankGenome :: [ProblemType] -> [EvaluatedGenome a] -> EvaluatedGenome a -> IntermediateRank a
rankGenome ptypes allGenomes genome =
    let this = snd genome
        -- dominating are better than this genome
        dominating = filter (\(_,other) -> dominates ptypes other this) allGenomes
        -- this genome is better than dominated ones
        dominated  = filter (\(_,other) -> dominates ptypes this other) allGenomes
    in  IntermediateRank (length dominating) dominated


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

-- | Given there is non-domination rank @rank_i@, and local crowding
-- distance @distance_i@ assigned to every individual @i@, the partial
-- order between individuals @i@ and @q@ is defined by relation
--
-- @i >: j@ if @rank_i < rank_j@ or (@rank_i = rank_j@ and @distance_i@
-- @>@ @distance_j@).
--
crowdedCompare :: RankedSolution a -> RankedSolution a -> Ordering
crowdedCompare (RankedSolution _ ranki disti) (RankedSolution _ rankj distj) =
    case (ranki < rankj, ranki == rankj, disti > distj) of
      (True, _, _) -> GT
      (_, True, True) -> GT
      (_, True, False) -> if disti == distj
                          then EQ
                          else LT
      _  -> LT


-- | Assign non-domination rank and crowding distances to all solutions.
rankAllSolutions :: [ProblemType] -> [EvaluatedGenome a] -> [RankedSolution a]
rankAllSolutions ptypes genomes =
    let -- non-dominated fronts
        fronts = nondominatedSort ptypes genomes
        -- for every non-dominated front
        frontsDists = map (crowdingDistances . map snd) fronts
        ranks = iterate (+1) 1
    in  concatMap rankedSolutions1 (zip3 fronts ranks frontsDists)
  where
    rankedSolutions1 :: ([EvaluatedGenome a], Int, [Double]) -> [RankedSolution a]
    rankedSolutions1 (front, rank, dists) =
        zipWith (\g d -> RankedSolution g rank d) front dists
