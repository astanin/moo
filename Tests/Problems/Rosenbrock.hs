{- Minimize Rosenbrock function using real-valued genetic algorithm.
   Optimal value x* = (1,...,1). F(x*) = 0.
-}

module Tests.Problems.Rosenbrock where

import Test.HUnit

import Text.Printf
import Data.List (intercalate)

import Tests.Common

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Selection (sortByFitness)


rosenbrock :: [Double] -> Double
rosenbrock xs = sum . map f $ zip xs (drop 1 xs)
  where
   f (x1, x2) = 100 * (x2 - x1^2)^2 + (x1 - 1)^2

testRosenbrock =
    "Rosenbrock 2D GM/UNDX" ~: do
      let tolerance = 1e-3
      let iterations = 500
      let problem = RealMinimize rosenbrock [(-10,10),(-20,20)]  [1.0, 1.0]
      let solver = solverReal problem 101 11 undx iterations
      (pop, dist) <- runSolverReal problem solver
      let best = takeGenome . head $ sortByFitness pop
      putStrLn $ "best:   " ++ (intercalate " " (map (printf "%.5f") best))
      putStrLn $ "error:  " ++ show dist
      assertBool ("error > " ++ show tolerance) (dist < tolerance)