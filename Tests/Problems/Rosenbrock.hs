{- Minimize Rosenbrock function using real-valued genetic algorithm.
   Optimal value x* = (1,...,1). F(x*) = 0.
-}

module Tests.Problems.Rosenbrock where

import Test.HUnit

import Text.Printf
import Data.List (intercalate)
import System.IO (hPutStrLn, stderr)

import Tests.Common

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Selection (sortByFitness)
import Moo.GeneticAlgorithm.Run (Cond(..))


pr = hPutStrLn stderr


rosenbrock :: [Double] -> Double
rosenbrock xs = sum . map f $ zip xs (drop 1 xs)
  where
   f (x1, x2) = 100 * (x2 - x1^2)^2 + (x1 - 1)^2


testRosenbrock = TestList
  [ "Rosenbrock 2D GM/UNDX/500 gens" ~: do
      let tolerance = 1e-3  -- solution error
      let maxiters = 500
      let problem = RealMinimize rosenbrock [(-10,10),(-20,20)]  [1,1]
      let solver = solverReal problem 101 11 undx (Iteration maxiters)
      (pop, dist) <- runSolverReal problem solver
      let best = takeGenome . head $ sortByFitness pop
      pr ""
      pr $ "best:    " ++ (intercalate " " (map (printf "%.5f") best))
      pr $ "error:   " ++ (printf "%.5g" dist)
      assertBool ("error >= " ++ show tolerance) (dist < tolerance)
  , "Rosenbrock 2D GM/SBX/min residual, max 500 gens" ~: do
      let tolerance = 1e-6  -- fitness residual
      let maxiters = 500
      let problem = RealMinimize rosenbrock [(-20,20),(-20,20)] [1,1]
      let stop = Iteration maxiters `Or` IfFitness ((>= -tolerance) . maximum)
      let solver = solverReal problem 101 11 sbx stop
      (pop, dist) <- runSolverReal problem solver
      let best = head $ sortByFitness pop
      let bestG = takeGenome best
      let bestF = takeFitness  best
      pr ""
      pr $ "best:    " ++ (intercalate " " (map (printf "%.5f") bestG))
      pr $ "fitness: " ++ (printf "%.5g" bestF)
      assertBool ("fitness < " ++ show (negate tolerance)) (bestF >= -tolerance)
  , "Rosenbrock 2D GM/BLX-0.5/min residual, max 500 gens" ~: do
      let tolerance = 1e-3  -- solution error
      let maxiters = 500
      let problem = RealMinimize rosenbrock [(-20,20),(-20,20)] [1,1]
      let stop = Iteration maxiters
      let solver = solverReal problem 101 11 blxa stop
      (pop, dist) <- runSolverReal problem solver
      let bestG = takeGenome . head $ sortByFitness pop
      pr ""
      pr $ "best:    " ++ (intercalate " " (map (printf "%.5f") bestG))
      pr $ "error:   " ++ (printf "%.5g" dist)
      assertBool ("error >= " ++ show tolerance) (dist < tolerance)
  ]
