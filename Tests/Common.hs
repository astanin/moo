{-# LANGUAGE BangPatterns #-}
module Tests.Common where

import Moo.GeneticAlgorithm.Run
import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random

import Data.List (foldl')
import Control.Monad (replicateM)

data RealProblem = RealMinimize {
      minimizeFunction :: [Double] -> Double  -- ^ function to minimize
    , minimizeVarRange :: [(Double, Double)]  -- ^ search space
    , minimizeSolution :: [Double]            -- ^ problem solution
    }


-- Unit Gaussian mutation, 1/2 per genome
gauss sigma nvars =
    let p = 0.5/fromIntegral nvars
    in  gaussianMutate p sigma


-- BLX-0.5 crossover
blxa = blendCrossover 0.5


-- UNDX crossover
undx = unimodalCrossoverRP


-- SBX crossover
sbx = simulatedBinaryCrossover 2


randomGenomesReal :: Int -> [(Double,Double)] -> Rand [Genome Double]
randomGenomesReal popsize ranges = replicateM popsize randomGenome
    where
      randomGenome = mapM (\varRange -> getRandomR varRange) ranges


data Solver a = Solver {
      s'popsize :: Int
    , s'elitesize :: Int
    , s'objective :: ObjectiveFunction a
    , s'select :: SelectionOp a
    , s'crossover :: CrossoverOp a
    , s'mutate :: MutationOp a
    , s'stopcond :: Cond a
    }


-- default solver for real-valued problems
solverReal :: RealProblem -> Int -> Int -> CrossoverOp Double -> Cond Double -> Solver Double
solverReal (RealMinimize f vranges sol) popsize elitesize crossover stopcond =
    let nvars = length vranges
        s = 0.1 * average (map (uncurry subtract) vranges)
        mutate = gauss s nvars
        fitness xs _ = f xs
        select = tournamentSelect Minimizing 3 (popsize - elitesize)
    in  Solver popsize elitesize fitness select crossover mutate stopcond


runSolverReal :: RealProblem
              -> Solver Double
              -> IO (Population Double, Double)
              -- ^ final population and euclidean distance from the known solution
runSolverReal problem solver = do
    let ptype = Minimizing
    let init = randomGenomesReal (s'popsize solver) (minimizeVarRange problem)
    let step = nextGeneration  ptype (s'objective solver)
               (s'select solver) (s'elitesize solver)
               (s'crossover solver) (s'mutate solver)
    let ga   = loop (s'stopcond solver) step
    pop <- runGA init ga
    let best = takeGenome . head $ bestFirst ptype pop
    let dist = sqrt . sum . map (^2) $ zipWith (-) best (minimizeSolution problem)
    return (pop, dist)


-- |Average
average :: (Num a, Fractional a) => [a] -> a
average = uncurry (/) . foldl' (\(!s, !c) x -> (s+x, c+1)) (0, 0)
