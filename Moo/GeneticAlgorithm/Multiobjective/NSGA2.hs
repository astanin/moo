{-# LANGUAGE Rank2Types #-}
{- |

NSGA-II. A Fast Elitist Non-Dominated Sorting Genetic
Algorithm for Multi-Objective Optimization.

Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. A. M. T. (2002). A
fast and elitist multiobjective genetic algorithm:
NSGA-II. Evolutionary Computation, IEEE Transactions on, 6(2),
182-197.

Functions to be used:

  'nsga2Ranking', 'nondominatedRanking',
  'stepNSGA2', 'stepNSGA2default'

The other functions are exported for testing only.

-}

module Moo.GeneticAlgorithm.Multiobjective.NSGA2 where


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Multiobjective.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Utilities (doCrossovers)
import Moo.GeneticAlgorithm.StopCondition (evalCond)
import Moo.GeneticAlgorithm.Selection (tournamentSelect, bestFirst)


import Control.Monad (forM_, (<=<))
import Data.Array (array, (!), elems)
import Data.Array.ST (runSTArray, newArray, readArray, writeArray)
import Data.Function (on)
import Data.List (sortBy, groupBy, transpose)


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
        -- (genome-idx, objective-idx) -> objective value
        ovTable = array ((0,0), (n-1, m-1))
                  [ ((i, objid), (pop !! i) !! objid)
                  | i <- [0..(n-1)], objid <- [0..(m-1)] ]
        -- calculate crowding distances
        distances = runSTArray $ do
          ss <- newArray (0, n-1) 0.0  -- initialize distances
          forM_ [0..(m-1)] $ \objid -> do    -- for every objective
            let ixs = sortByObjective objid pop
              -- for all inner points
            forM_ (zip3 ixs (drop 1 ixs) (drop 2 ixs)) $ \(iprev, i, inext) -> do
              sum_of_si <- readArray ss i
              let si = (ovTable ! (inext, objid)) - (ovTable ! (iprev, objid))
              writeArray ss i (sum_of_si + si)
            writeArray ss (head ixs) inf   -- boundary points have infinite cuboids
            writeArray ss (last ixs) inf
          return ss
    in elems distances
  where
    sortByObjective :: Int -> [[Objective]] -> [Int]
    sortByObjective i pop = sortIndicesBy (compare `on` (!! i)) pop

-- | Given there is non-domination rank @rank_i@, and local crowding
-- distance @distance_i@ assigned to every individual @i@, the partial
-- order between individuals @i@ and @q@ is defined by relation
--
-- @i ~ j@ if @rank_i < rank_j@ or (@rank_i = rank_j@ and @distance_i@
-- @>@ @distance_j@).
--
crowdedCompare :: RankedSolution a -> RankedSolution a -> Ordering
crowdedCompare (RankedSolution _ ranki disti) (RankedSolution _ rankj distj) =
    case (ranki < rankj, ranki == rankj, disti > distj) of
      (True, _, _) -> LT
      (_, True, True) -> LT
      (_, True, False) -> if disti == distj
                          then EQ
                          else GT
      _  -> GT


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


-- | To every genome in the population, assign a single objective
-- value according to its non-domination rank. This ranking is
-- supposed to be used once in the beginning of the NSGA-II algorithm.
--
-- 'nondominatedRanking' reorders the genomes.
nondominatedRanking
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn     -- ^ list of @problems@
    -> [Genome a]                   -- ^ a population of raw @genomes@
    -> [(Genome a, Objective)]
nondominatedRanking problems genomes =
    let ptypes = map fst problems
        egs = evalAllObjectives problems genomes
        fronts = nondominatedSort ptypes egs
        ranks = concatMap assignRanks (zip fronts (iterate (+1) 1))
    in  ranks
  where
    assignRanks :: ([EvaluatedGenome a], Int) -> [(Genome a, Objective)]
    assignRanks (gs, r) = map (\(eg, rank) -> (fst eg, fromIntegral rank)) $ zip gs (repeat r)


-- | To every genome in the population, assign a single objective value
-- according to its non-domination rank and local crowding distance
-- (i.e. sort the population with NSGA-II crowded comparision
-- operator, and return sequence positions).
--
-- 'nsga2Ranking' reorder the genomes.
nsga2Ranking
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn     -- ^ a list of @problems@
    -> [Genome a]                   -- ^ a population of raw @genomes@
    -> [(Genome a, Objective)]
nsga2Ranking problems genomes =
    let ptypes = map fst problems
        evaledGenomes = evalAllObjectives problems genomes
        rankedGenomes = rankAllSolutions ptypes evaledGenomes
        ranks = map (+1) $ sortIndicesBy crowdedCompare rankedGenomes
    in  zip (map (fst . rs'genome) rankedGenomes)
            (map fromIntegral ranks)


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


sortIndicesBy :: (a -> a -> Ordering) -> [a] -> [Int]
sortIndicesBy cmp xs = map snd $ sortBy (cmp `on` fst) (zip xs (iterate (+1) 0))

-- | A single step of the NSGA-II algorithm (Non-Dominated Sorting
-- Genetic Algorithm for Multi-Objective Optimization).
--
-- The next population is selected from a common pool of parents and
-- their children minimizing the non-domination rank and maximizing
-- the crowding distance within the same rank.
-- The first generation of children is produced without taking
-- crowding into account.
--
-- Reference:
-- Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. A. M. T. (2002). A
-- fast and elitist multiobjective genetic algorithm:
-- NSGA-II. Evolutionary Computation, IEEE Transactions on, 6(2),
-- 182-197.
--
-- Deb et al. used a binary tournament selection, base on crowded
-- comparison operator. To achieve the same effect, use
-- 'stepNSGA2default' (or 'stepNSGA2' with 'tournamentSelect'
-- @Minimizing 2 n@, where @n@ is the size of the population).
--
stepNSGA2
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn    -- ^ a list of @objective@ functions
    -> SelectionOp a
    -> CrossoverOp a
    -> MutationOp a
    -> StepGA Rand a
stepNSGA2 problems select crossover mutate stop input = do
  case input of
    (Left rawgenomes) ->
        stepNSGA2'firstGeneration problems select crossover mutate stop rawgenomes
    (Right rankedgenomes) ->
        stepNSGA2'nextGeneration problems select crossover mutate stop rankedgenomes


-- | A single step of NSGA-II algorithm with binary tournament selection.
-- See also 'stepNSGA2'.
stepNSGA2default
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn    -- ^ a list of @objective@ functions
    -> CrossoverOp a
    -> MutationOp a
    -> StepGA Rand a
stepNSGA2default problems crossover mutate stop popstate =
    let n = either length length popstate
        select = tournamentSelect Minimizing 2 n
    in  stepNSGA2 problems select crossover mutate stop popstate


stepNSGA2'firstGeneration
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn    -- ^ a list of @objective@ functions
    -> SelectionOp a
    -> CrossoverOp a
    -> MutationOp a
    -> Cond a
    -> [Genome a]
    -> Rand (StepResult [Phenotype a])
stepNSGA2'firstGeneration problems select crossover mutate stop genomes = do
  let objective = nondominatedRanking problems
  let phenotypes = evalObjective objective genomes
  if (evalCond stop phenotypes)
      then return $ StopGA phenotypes  -- stop before the first iteration
      else do
        let popsize = length phenotypes
        selected <- (shuffle <=< select) phenotypes
        newgenomes <- (mapM mutate) <=< (flip doCrossovers crossover) $ selected
        let pool = newgenomes ++ genomes
        stepNSGA2'poolSelection problems popsize stop pool


-- | Use normal selection, crossover, mutation to produce new
-- children.  Select from a common pool of parents and children the
-- best according to the least non-domination rank and crowding.
stepNSGA2'nextGeneration
     :: forall fn a . ObjectiveFunction fn a
     => MultiObjectiveProblem fn   -- ^ a list of objective functions
     -> SelectionOp a
     -> CrossoverOp a
     -> MutationOp a
     -> Cond a
     -> [Phenotype a]
     -> Rand (StepResult [Phenotype a])
stepNSGA2'nextGeneration problems select crossover mutate stop rankedgenomes = do
  let popsize = length rankedgenomes
  selected <- select rankedgenomes
  newgenomes <- (mapM mutate) <=< flip doCrossovers crossover <=< shuffle $ selected
  let pool = (map takeGenome rankedgenomes) ++ newgenomes
  stepNSGA2'poolSelection problems popsize stop pool


-- | Take a pool of phenotypes of size 2N, ordered by the crowded
-- comparison operator, and select N best. Check if the stop condition
-- is satisfied.
stepNSGA2'poolSelection
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn   -- ^ a list of @objective@ functions
    -> Int                  -- ^ @n@, the number of solutions to select
    -> Cond a               -- ^ @stop@ condition
    -> [Genome a]           -- ^ a common pool of parents and their children
    -> Rand (StepResult [Phenotype a])
stepNSGA2'poolSelection problems n stop pool = do
    let objective = nsga2Ranking problems
    let rankedgenomes = bestFirst Minimizing $ evalObjective objective pool
    let selected = take n rankedgenomes  -- :: [Phenotype a]
    return $ if evalCond stop selected
             then StopGA selected
             else ContinueGA selected
