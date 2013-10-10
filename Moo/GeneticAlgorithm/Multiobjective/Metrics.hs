{-# LANGUAGE RankNTypes #-}
{- | Performance metrics for multiobjective problems.

-}

module Moo.GeneticAlgorithm.Multiobjective.Metrics where


import Data.List (tails, sortBy)
import Data.Function (on)


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Multiobjective.Types
import Moo.GeneticAlgorithm.Multiobjective.NSGA2


type Point = [Double]


-- | Calculate the hypervolume indicator using WFG algorithm.
--
-- Reference:
-- While, L., Bradstreet, L., & Barone, L. (2012). A fast way of
-- calculating exact hypervolumes. Evolutionary Computation, IEEE
-- Transactions on, 16(1), 86-95.
--
hypervolume :: forall fn a . ObjectiveFunction fn a
            => MultiObjectiveProblem fn   -- ^ multiobjective problem @mop@
            -> [Objective]                -- ^ reference point (the worst point)
            -> [MultiPhenotype a]         -- ^ a set of solutions to evaluate
            -> Double                     -- ^ hypervolume
hypervolume mop refPoint solutions =
    let ptypes = map fst mop :: [ProblemType]
        points = map takeObjectiveValues solutions
    in  wfgHypervolume_sort 0 ptypes refPoint points


-- | Basic (non-optimized) WFG algorithm to calculate hypervolume.
--
-- Reference: While et al. (2012).
wfgHypervolume :: [ProblemType]  -- ^ problem types
               -> Point          -- ^ reference point (the @worst@ point)
               -> [Point]        -- ^ a set of points
               -> Double
wfgHypervolume ptypes worst pts =
    let ptsAndTails = zip pts (drop 1 (tails pts)) :: [(Point, [Point])]
        exclusiveHvs = map
                       (\(pt, rest) -> exclusiveHypervolume ptypes worst pt rest)
                       ptsAndTails
    in  sum exclusiveHvs


-- | WFG algorithm to calculate hypervolume with sorting optimization.
wfgHypervolume_sort :: Int            -- ^ index of the objective to sort
                    -> [ProblemType]  -- ^ problem types
                    -> Point          -- ^ reference point (the @worst@ point)
                    -> [Point]        -- ^ a set of points
                    -> Double
wfgHypervolume_sort k ptypes worst pts
    | null ptypes || length ptypes <= k || k < 0 =
        wfgHypervolume_sort 0 ptypes worst pts  -- bad input, sort the first objective
    | otherwise =
        let ptype = ptypes !! k
            pts' = sortBy (flip compare `on` get ptype k) pts
        in  wfgHypervolume ptypes worst pts'
    where
      get :: ProblemType -> Int -> [Double] -> Double
      get Minimizing k objvals
          | length objvals > k = objvals !! k
          | otherwise          = inf
      get Maximizing k objvals
          | length objvals > k = objvals !! k
          | otherwise          = - inf
      inf :: Double
      inf = 1/0


-- | Construct a limited set (a step of the WFG algorithm).
--
-- @
--     limitSet(S, p) = { limit(x, p) | x \in S }
--     where limit(<s1, ..., sn>, <p1, ..., pn>) = < worse(s1,p1), ..., worse(sn, pn)>.
-- @
limitSet :: [ProblemType] -- ^ problem types
         -> Point         -- ^ reference point
         -> [Point]       -- ^ original set
         -> [Point]       -- ^ limited set
limitSet ptypes refPoint =
    map (zipWith3 worst ptypes refPoint)
  where
    worst :: ProblemType -> Double -> Double -> Double
    worst Minimizing x y | x > y     = x
                         | otherwise = y
    worst Maximizing x y | x < y     = x
                         | otherwise = y


-- | Construct a non-dominated subset (a step of the WFG algorithm).
nondominatedSet :: [ProblemType]  -- ^ problem types
                -> [Point]        -- ^ original set
                -> [Point]        -- ^ a non-dominated subset
nondominatedSet ptypes points =
    let dominates = domination ptypes
        dummySolutions = map (\objvals -> ([], objvals)) points :: [MultiPhenotype Double]
        fronts = nondominatedSort dominates dummySolutions :: [[MultiPhenotype Double]]
    in  case fronts of
          (nds:_) -> map takeObjectiveValues nds
          _       -> []


-- | Calculate inclusive hypervolume of a point @p@ (the size of the
-- part of the objective space dominated by @p@ alone).
inclusiveHypervolume :: [ProblemType]  -- ^ problem types
                     -> Point          -- ^ reference point (the @worst@ point)
                     -> Point          -- ^ a point @p@ to evaluate
                     -> Double         -- ^ inclusive hypervolume
inclusiveHypervolume ptypes worst p =
    product $ zipWith3 hyperside ptypes worst p
 where
    hyperside :: ProblemType -> Double -> Double -> Double
    hyperside Minimizing upper x = pos $ upper - x
    hyperside Maximizing lower x = pos $ x - lower
    -- Positive part: to truncate the hypervolume if an unsuitable
    -- reference point is given (not the worst one possible)
    pos :: Double -> Double
    pos x = 0.5 * (x + abs x)


-- | Calculate exclusive hypervolume of a point @p@ relative to the
-- @underlying@ set (the size of the part of the objective space that
-- is dominated by @p@, but is not dominated by any member of the
-- @underlying@ set).
exclusiveHypervolume :: [ProblemType]  -- ^ problem types
                     -> Point          -- ^ reference point (the @worst@ point)
                     -> Point          -- ^ a point @p@ to evaluate
                     -> [Point]        -- ^ an @underlying@ set of points
                     -> Double         -- ^ exclusive hypervolume
exclusiveHypervolume ptypes worst p underlying =
    let inclusiveHv = inclusiveHypervolume ptypes worst p
        nds = nondominatedSet ptypes $ limitSet ptypes p underlying
        underlyingHv = wfgHypervolume ptypes worst nds
    in  inclusiveHv - underlyingHv
