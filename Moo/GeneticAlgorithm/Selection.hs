{- |

Selection operators for genetic algorithms.

-}

module Moo.GeneticAlgorithm.Selection
  (
    rouletteSelect
  , tournamentSelect
  -- ** Scaling and niching
  , withPopulationTransform
  , withScale
  , rankScale
  , withFitnessSharing
  -- ** Sorting
  , bestFirst
  ) where


import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Niching (fitnessSharing)


import Control.Monad (liftM, replicateM)
import Control.Arrow (second)
import Data.List (sortBy)
import Data.Function (on)


-- | Apply given scaling or other transform to population before selection.
withPopulationTransform :: (Population a -> Population a) -> SelectionOp a -> SelectionOp a
withPopulationTransform transform select = \pop -> select (transform pop)


-- | Transform objective function values before seletion.
withScale :: (Objective -> Objective) -> SelectionOp a -> SelectionOp a
withScale f select =
    let scale = map (second f)
    in  withPopulationTransform scale select

-- | Replace objective function values in the population with their
-- ranks.  For a population of size @n@, a genome with the best value
-- of objective function has rank @n' <= n@, and a genome with the
-- worst value of objective function gets rank @1@.
--
-- 'rankScale' may be useful to avoid domination of few super-genomes
-- in 'rouletteSelect' or to apply 'rouletteSelect' when an objective
-- function is not necessarily positive.
rankScale :: ProblemType -> Population a -> Population a
rankScale problem pop =
    let sorted = bestFirst (opposite problem) pop  -- worst first
        worst = takeObjectiveValue . head $ sorted
    in  ranks 1 worst sorted
    where
      ranks _ _ [] = []
      ranks rank worst ((genome,objective):rest)
          | worst == objective  = (genome,rank)   : ranks rank worst rest
          | otherwise           = (genome,rank+1) : ranks (rank+1) objective rest
      opposite Minimizing = Maximizing
      opposite Maximizing = Minimizing


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
withFitnessSharing ::
    (Phenotype a -> Phenotype a -> Double)  -- ^ distance function
    -> Double  -- ^ niche radius
    -> Double  -- ^ niche compression exponent @alpha@ (usually 1)
    -> ProblemType   -- ^ type of the optimization problem
    -> (SelectionOp a -> SelectionOp a)
withFitnessSharing dist r alpha ptype =
    withPopulationTransform (fitnessSharing dist r alpha ptype)


-- |Objective-proportionate (roulette-wheel) selection: select @n@
-- random items with each item's chance of being selected is
-- proportional to its objective function (fitness).
-- Objective function should be non-negative.
rouletteSelect :: Int -> SelectionOp a
rouletteSelect n xs = replicateM n roulette1
  where
  fs = map takeObjectiveValue xs
  xs' = zip xs (scanl1 (+) fs)
  sumScores = (snd . last) xs'
  roulette1 = do
    rand <- (sumScores*) `liftM` getDouble
    return $ (fst . head . dropWhile ((rand >) . snd)) xs'

-- |Performs tournament selection among @size@ individuals and
-- returns the winner. Repeat @n@ times.
tournamentSelect :: ProblemType  -- ^ type of the optimization problem
                 -> Int -- ^ size of the tournament group
                 -> Int -- ^ how many tournaments to run
                 -> SelectionOp a
tournamentSelect problem size n xs = replicateM n tournament1
  where
  tournament1 = do
    contestants <- randomSample size xs
    let winner = head $ bestFirst problem contestants
    return winner

-- | Sort population by decreasing objective function (also known as
-- fitness for maximization problems). The genomes with the highest
-- fitness are put in the head of the list.
sortByFitnessDesc :: Population a -> Population a
sortByFitnessDesc = sortBy (flip compare `on` snd)

-- | Sort population by increasing objective function (also known as
-- cost for minimization problems). The genomes with the smallest
-- cost are put in the head of the list.
sortByCostAsc :: Population a -> Population a
sortByCostAsc = sortBy (compare `on` snd)

-- | Reorders a list of individual solutions,
-- by putting the best in the head of the list.
bestFirst :: ProblemType -> Population a -> Population a
bestFirst Maximizing = sortByFitnessDesc
bestFirst Minimizing = sortByCostAsc
