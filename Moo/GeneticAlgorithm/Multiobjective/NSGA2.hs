{-# LANGUAGE Rank2Types, ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{- |

NSGA-II. A Fast Elitist Non-Dominated Sorting Genetic
Algorithm for Multi-Objective Optimization.

Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. A. M. T. (2002). A
fast and elitist multiobjective genetic algorithm:
NSGA-II. Evolutionary Computation, IEEE Transactions on, 6(2),
182-197.

Functions to be used:

  'stepNSGA2', 'stepNSGA2bt',
  'stepConstrainedNSGA2', 'stepConstrainedNSGA2bt'

The other functions are exported for testing only.

-}

module Moo.GeneticAlgorithm.Multiobjective.NSGA2 where


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Multiobjective.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Utilities (doCrossovers)
import Moo.GeneticAlgorithm.Selection (tournamentSelect)
import Moo.GeneticAlgorithm.Constraints
import Moo.GeneticAlgorithm.Run (makeStoppable)


import Control.Monad (forM_, (<=<), when, liftM)
import Control.Monad.ST (ST)
import Data.Array (array, (!), elems, listArray)
import Data.Array.ST (STArray, runSTArray, newArray, readArray, writeArray, getElems, getBounds)
import Data.Function (on)
import Data.List (sortBy)
import Data.STRef


-- | Returns @True@ if the first solution dominates the second one in
-- some sense.
type DominationCmp a = MultiPhenotype a -> MultiPhenotype a -> Bool


-- | A solution @p@ dominates another solution @q@ if at least one 'Objective'
-- values of @p@ is better than the respective value of @q@, and the other
-- are not worse.
domination :: [ProblemType] -- ^ problem types per every objective
           -> DominationCmp a
domination ptypes p q =
    let pvs = takeObjectiveValues p
        qvs = takeObjectiveValues q
        pqs = zip3 ptypes pvs qvs
        qps = zip3 ptypes qvs pvs
    in  (any better1 pqs) && (all (not . better1) qps)
  where
    better1 :: (ProblemType, Objective, Objective) -> Bool
    better1 (Minimizing, pv, qv) = pv < qv
    better1 (Maximizing, pv, qv) = pv > qv


-- | A solution p is said to constrain-dominate a solution q, if any of the
-- following is true: 1) Solution p is feasible and q is not. 2) Solutions
-- p and q are both infeasible but solution p has a smaller overall constraint
-- violation. 3) Solutions p and q are feasible, and solution p dominates solution q.
--
-- Reference: (Deb, 2002).
constrainedDomination :: (Real b, Real c)
                      => [Constraint a b]  -- ^ constraints
                      -> ([Constraint a b] -> Genome a -> c)  -- ^ non-negative degree of violation
                      -> [ProblemType]     -- ^ problem types per every objective
                      -> DominationCmp a
constrainedDomination constraints violation ptypes p q =
    let pok = isFeasible constraints p
        qok = isFeasible constraints q
    in  case (pok, qok) of
          (True, True) -> domination ptypes p q
          (False, True) -> False
          (True, False) -> True
          (False, False) ->
              let pviolation = violation constraints (takeGenome p)
                  qviolation = violation constraints (takeGenome q)
              in  pviolation < qviolation


-- | Solution and its non-dominated rank and local crowding distance.
data RankedSolution a = RankedSolution {
      rs'phenotype :: MultiPhenotype a
    , rs'nondominationRank :: Int  -- ^ @0@ is the best
    , rs'localCrowdingDistnace :: Double  -- ^ @Infinity@ for less-crowded boundary points
    } deriving (Show, Eq)


-- | Fast non-dominated sort from (Deb et al. 2002).
-- It is should be O(m N^2), with storage requirements of O(N^2).
nondominatedSort :: DominationCmp a -> [MultiPhenotype a] -> [[MultiPhenotype a]]
nondominatedSort dominates = nondominatedSortFast dominates


-- | This is a direct translation of the pseudocode from (Deb et al. 2002).
nondominatedSortFast :: DominationCmp a -> [MultiPhenotype a] -> [[MultiPhenotype a]]
nondominatedSortFast dominates gs =
    let n = length gs   -- number of genomes
        garray = listArray (0, n-1) gs
        fronts = runSTArray $ do
                     -- structure of sp array:
                     -- sp [pi][0]    -- n_p, number of genomes dominating pi-th genome
                     -- sp [pi][1]    -- size of S_p, how many genomes pi-th genome dominates
                     -- sp [pi][2..]  -- indices of the genomes dominated by pi-th genome
                     --               -- where pi in [0..n-1]
                     --
                     -- structure of the fronts array:
                     -- fronts [0][i]        -- size of the i-th front
                     -- fronts [1][start..start+fsizes[i]-1] -- indices of the elements of the i-th front
                     --                                      -- where start = sum (take (i-1) fsizes)
                     --
                     -- domination table
                     sp <- newArray ((0,0), (n-1, (n+2)-1)) 0 :: ST s (STArray s (Int,Int) Int)
                     -- at most n fronts with 1 element each
                     fronts <- newArray ((0,0), (1,n-1)) 0 :: ST s (STArray s (Int,Int) Int)
                     forM_ (zip gs [0..]) $ \(p, pi) -> do  -- for each p in P
                       forM_ (zip gs [0..]) $ \(q, qi) -> do  -- for each q in P
                         when ( p `dominates` q ) $
                              -- if p dominates q, include q in S_p
                              includeInSp sp pi qi
                         when ( q `dominates` p) $
                              -- if q dominates p, increment n_p
                              incrementNp sp pi
                       np <- readArray sp (pi, 0)
                       when (np == 0) $
                            addToFront 0 fronts pi
                     buildFronts sp fronts 0
        frontSizes = takeWhile (>0) . take n $ elems fronts
        frontElems = map (\i -> garray ! i) . drop n $ elems fronts
    in  splitAll frontSizes frontElems

  where

    includeInSp sp pi qi = do
      oldspsize <- readArray sp (pi, 1)
      writeArray sp (pi, 2 + oldspsize) qi
      writeArray sp (pi, 1) (oldspsize + 1)

    incrementNp sp pi = do
      oldnp <- readArray sp (pi, 0)
      writeArray sp (pi, 0) (oldnp + 1)

    -- size of the i-th front
    frontSize fronts i =
        readArray fronts (0, i)

    frontStartIndex fronts frontno = do
      -- start = sum (take (frontno-1) fsizes)
      startref <- newSTRef 0
      forM_ [0..(frontno-1)] $ \i -> do
          oldstart <- readSTRef startref
          l <- frontSize fronts i
          writeSTRef startref (oldstart + l)
      readSTRef startref

    -- adjust fronts array by updating frontno-th front size and appending
    -- pi to its elements; frontno should be the last front!
    addToFront frontno fronts pi = do
      -- update i-th front size and write an index in the correct position
      start <- frontStartIndex fronts frontno
      sz <- frontSize fronts frontno
      writeArray fronts (1, start + sz) pi
      writeArray fronts (0, frontno) (sz + 1)

    -- elements of the i-th front
    frontElems fronts i = do
      start <- frontStartIndex fronts i
      sz <- frontSize fronts i
      felems <- newArray (0, sz-1) (-1) :: ST s (STArray s Int Int)
      forM_ [0..sz-1] $ \elix ->
          readArray fronts (1, start+elix) >>= writeArray felems elix
      getElems felems

    -- elements which are dominated by the element pi
    dominatedSet sp pi = do
      sz <- readArray sp (pi, 1)
      delems <- newArray (0, sz-1) (-1) :: ST s (STArray s Int Int)
      forM_ [0..sz-1] $ \elix ->
          readArray sp (pi, 2+elix) >>= writeArray delems elix
      getElems delems

    buildFronts sp fronts i = do
      maxI <- (snd . snd) `liftM` getBounds fronts
      if (i >= maxI || i < 0) -- all fronts are singletons and the last is already built
         then return fronts
         else do

      fsz <- frontSize fronts i
      if fsz <= 0
         then return fronts
         else do

      felems <- frontElems fronts i
      forM_ felems $ \pi -> do   -- for each member p in F_i
          dominated <- dominatedSet sp pi
          forM_ dominated $ \qi -> do  -- modify each member from the set S_p
               nq <- liftM (+ (-1::Int)) $ readArray sp (qi, 0)  -- decrement n_q by one
               writeArray sp (qi, 0) nq
               when (nq <= 0) $  -- if n_q is zero, q is a member of the next front
                    addToFront (i+1) fronts qi
      buildFronts sp fronts (i+1)

    splitAll [] _ = []
    splitAll _ [] = []
    splitAll (sz:szs) els =
        let (front, rest) = splitAt sz els
        in  front : (splitAll szs rest)


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
-- Return a list of non-domination fronts.
rankAllSolutions :: DominationCmp a -> [MultiPhenotype a] -> [[RankedSolution a]]
rankAllSolutions dominates genomes =
    let -- non-dominated fronts
        fronts = nondominatedSort dominates genomes
        -- for every non-dominated front
        frontsDists = map (crowdingDistances . map snd) fronts
        ranks = iterate (+1) 1
    in  map rankedSolutions1 (zip3 fronts ranks frontsDists)
  where
    rankedSolutions1 :: ([MultiPhenotype a], Int, [Double]) -> [RankedSolution a]
    rankedSolutions1 (front, rank, dists) =
        zipWith (\g d -> RankedSolution g rank d) front dists


-- | To every genome in the population, assign a single objective
-- value according to its non-domination rank. This ranking is
-- supposed to be used once in the beginning of the NSGA-II algorithm.
--
-- Note: 'nondominatedRanking' reorders the genomes.
nondominatedRanking
    :: forall fn a . ObjectiveFunction fn a
    => DominationCmp a
    -> MultiObjectiveProblem fn     -- ^ list of @problems@
    -> [Genome a]                   -- ^ a population of raw @genomes@
    -> [(Genome a, Objective)]
nondominatedRanking dominates problems genomes =
    let egs = evalAllObjectives problems genomes
        fronts = nondominatedSort dominates egs
        ranks = concatMap assignRanks (zip fronts (iterate (+1) 1))
    in  ranks
  where
    assignRanks :: ([MultiPhenotype a], Int) -> [(Genome a, Objective)]
    assignRanks (gs, r) = map (\(eg, rank) -> (fst eg, fromIntegral rank)) $ zip gs (repeat r)


-- | To every genome in the population, assign a single objective value
-- equal to its non-domination rank, and sort genomes by the decreasing
-- local crowding distance within every rank
-- (i.e. sort the population with NSGA-II crowded comparision
-- operator)
nsga2Ranking
    :: forall fn a . ObjectiveFunction fn a
    => DominationCmp a
    -> MultiObjectiveProblem fn    -- ^ a list of @objective@ functions
    -> Int                          -- ^ @n@, number of top-ranked genomes to select
    -> [Genome a]                   -- ^ a population of raw @genomes@
    -> [(MultiPhenotype a, Double)] -- ^ selected genomes with their non-domination ranks
nsga2Ranking dominates problems n genomes =
    let evaledGenomes = evalAllObjectives problems genomes
        fronts = rankAllSolutions dominates evaledGenomes
        frontSizes = map length fronts
        nFullFronts = length . takeWhile (< n) $ scanl1 (+) frontSizes
        partialSize = n - (sum (take nFullFronts frontSizes))
        (frontsFull, frontsPartial) = splitAt nFullFronts fronts
        fromFullFronts = concatMap (map assignRank) frontsFull
        fromPartialFront = concatMap (map assignRank
                                      . take partialSize
                                      . sortBy crowdedCompare) $
                           take 1 frontsPartial
    in  fromFullFronts ++ fromPartialFront
  where
    assignRank eg =
        let r = fromIntegral $ rs'nondominationRank eg
            phenotype = rs'phenotype $ eg
        in  (phenotype, r)


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
-- Every solution is assigned a single objective value which is its
-- sequence number after sorting with the crowded comparison operator.
-- The smaller value corresponds to solutions which are not worse
-- the one with the bigger value. Use 'evalAllObjectives' to restore
-- individual objective values.
--
-- Reference:
-- Deb, K., Pratap, A., Agarwal, S., & Meyarivan, T. A. M. T. (2002). A
-- fast and elitist multiobjective genetic algorithm:
-- NSGA-II. Evolutionary Computation, IEEE Transactions on, 6(2),
-- 182-197.
--
-- Deb et al. used a binary tournament selection, base on crowded
-- comparison operator. To achieve the same effect, use
-- 'stepNSGA2bt' (or 'stepNSGA2' with 'tournamentSelect'
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
  let dominates = domination (map fst problems)
  case input of
    (Left _) ->  -- raw genomes => it's the first generation
        stepNSGA2'firstGeneration dominates problems select crossover mutate stop input
    (Right _) ->  -- ranked genomes => it's the second or later generation
        stepNSGA2'nextGeneration dominates problems select crossover mutate stop input


-- | A single step of NSGA-II algorithm with binary tournament selection.
-- See also 'stepNSGA2'.
stepNSGA2bt
    :: forall fn a . ObjectiveFunction fn a
    => MultiObjectiveProblem fn    -- ^ a list of @objective@ functions
    -> CrossoverOp a
    -> MutationOp a
    -> StepGA Rand a
stepNSGA2bt problems crossover mutate stop popstate =
    let n = either length length popstate
        select = tournamentSelect Minimizing 2 n
    in  stepNSGA2 problems select crossover mutate stop popstate


-- | A single step of the constrained NSGA-II algorithm, which uses a
-- constraint-domination rule:
--
-- “A solution @i@ is said to constrain-dominate a solution @j@, if any of the
-- following is true: 1) Solution @i@ is feasible and @j@ is not. 2) Solutions
-- @i@ and @j@ are both infeasible but solution @i@ has a smaller overall constraint
-- violation. 3) Solutions @i@ and @j@ are feasible, and solution @i@ dominates solution @j@.”
--
-- Reference: (Deb, 2002).
--
stepConstrainedNSGA2
    :: forall fn a b c . (ObjectiveFunction fn a, Real b, Real c)
    => [Constraint a b]                     -- ^ constraints
    -> ([Constraint a b] -> Genome a -> c)  -- ^ non-negative degree of violation
    -> MultiObjectiveProblem fn             -- ^ a list of @objective@ functions
    -> SelectionOp a
    -> CrossoverOp a
    -> MutationOp a
    -> StepGA Rand a
stepConstrainedNSGA2 constraints violation problems select crossover mutate stop input = do
  let dominates = constrainedDomination constraints violation (map fst problems)
  case input of
    (Left _) ->
        stepNSGA2'firstGeneration dominates problems select crossover mutate stop input
    (Right _) ->
        stepNSGA2'nextGeneration dominates problems select crossover mutate stop input


-- | A single step of the constrained NSGA-II algorithm with binary tournament
-- selection. See also 'stepConstrainedNSGA2'.
stepConstrainedNSGA2bt
    :: forall fn a b c . (ObjectiveFunction fn a, Real b, Real c)
    => [Constraint a b]                     -- ^ constraints
    -> ([Constraint a b] -> Genome a -> c)  -- ^ non-negative degree of violation
    -> MultiObjectiveProblem fn             -- ^ a list of @objective@ functions
    -> CrossoverOp a
    -> MutationOp a
    -> StepGA Rand a
stepConstrainedNSGA2bt constraints violation problems crossover mutate stop popstate =
  let n = either length length popstate
      tournament = tournamentSelect Minimizing 2 n
  in  stepConstrainedNSGA2 constraints violation problems tournament crossover mutate stop popstate


stepNSGA2'firstGeneration
    :: forall fn a . ObjectiveFunction fn a
    => DominationCmp a
    -> MultiObjectiveProblem fn    -- ^ a list of @objective@ functions
    -> SelectionOp a
    -> CrossoverOp a
    -> MutationOp a
    -> StepGA Rand a
stepNSGA2'firstGeneration dominates problems select crossover mutate = do
  let objective = nondominatedRanking dominates problems
  makeStoppable objective $ \phenotypes -> do
    let popsize = length phenotypes
    let genomes = map takeGenome phenotypes
    selected <- liftM (map takeGenome) $ (shuffle <=< select) phenotypes
    newgenomes <- (mapM mutate) <=< (flip doCrossovers crossover) $ selected
    let pool = newgenomes ++ genomes
    return $ stepNSGA2'poolSelection dominates problems popsize pool


-- | Use normal selection, crossover, mutation to produce new
-- children.  Select from a common pool of parents and children the
-- best according to the least non-domination rank and crowding.
stepNSGA2'nextGeneration
     :: forall fn a . ObjectiveFunction fn a
     => DominationCmp a
     -> MultiObjectiveProblem fn   -- ^ a list of objective functions
     -> SelectionOp a
     -> CrossoverOp a
     -> MutationOp a
     -> StepGA Rand a
stepNSGA2'nextGeneration dominates problems select crossover mutate = do
  -- nextGeneration is never called with raw genomes,
  -- => dummyObjective is never evaluated;
  -- nondominatedRanking is required to type-check
  let dummyObjective = nondominatedRanking dominates problems
  makeStoppable dummyObjective $ \rankedgenomes -> do
    let popsize = length rankedgenomes
    selected <- liftM (map takeGenome) $ select rankedgenomes
    newgenomes <- (mapM mutate) <=< flip doCrossovers crossover <=< shuffle $ selected
    let pool = (map takeGenome rankedgenomes) ++ newgenomes
    return $ stepNSGA2'poolSelection dominates problems popsize pool


-- | Take a pool of phenotypes of size 2N, ordered by the crowded
-- comparison operator, and select N best.
stepNSGA2'poolSelection
    :: forall fn a . ObjectiveFunction fn a
    => DominationCmp a
    -> MultiObjectiveProblem fn    -- ^ a list of @objective@ functions
    -> Int                         -- ^ @n@, the number of solutions to select
    -> [Genome a]                  -- ^ @pool@ of genomes to select from
    -> [Phenotype a]               -- ^ @n@ best phenotypes
stepNSGA2'poolSelection dominates problems n pool =
    -- nsga2Ranking returns genomes properly sorted already
    let rankedgenomes = let grs = nsga2Ranking dominates problems n pool
                        in  map (\(mp,r) -> (takeGenome mp, r)) grs
        selected = take n rankedgenomes  -- :: [Phenotype a]
    in  selected
