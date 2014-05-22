{- Minimize Rosenbrock function using real-valued genetic algorithm.
   Optimal value x* = (1,...,1). F(x*) = 0.
-}

module Tests.Problems.Rosenbrock where

import Test.HUnit

import Text.Printf
import Data.List (intercalate)
import System.IO (hPutStrLn, stderr)
import Control.Monad (replicateM)

import Tests.Common

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Selection
import Moo.GeneticAlgorithm.Run
import Moo.GeneticAlgorithm.Random

pr _ = return ()
-- pr = hPutStrLn stderr


rosenbrock :: [Double] -> Double
rosenbrock xs = sum . map f $ zip xs (drop 1 xs)
  where
   f (x1, x2) = 100 * (x2 - x1^2)^2 + (x1 - 1)^2


testRosenbrock = TestList
  [ "Rosenbrock 2D GM/UNDX/500 gens" ~: do
      let tolerance = 1e-3  -- solution error
      let maxiters = 500
      let problem = RealMinimize rosenbrock [(-10,10),(-20,20)]  [1,1]
      let solver = solverReal problem 101 11 undx (Generations maxiters)
      (pop, dist) <- runSolverReal problem solver
      let best = takeGenome . head $ bestFirst Minimizing pop
      pr ""
      pr $ "best:    " ++ (intercalate " " (map (printf "%.5f") best))
      pr $ "error:   " ++ (printf "%.5g" dist)
      assertBool ("error >= " ++ show tolerance) (dist < tolerance)
  , "Rosenbrock 2D GM/SBX/min residual, max 500 gens" ~: do
      let tolerance = 1e-6  -- objective residual
      let maxiters = 500
      let problem = RealMinimize rosenbrock [(-20,20),(-20,20)] [1,1]
      let stop = Generations maxiters `Or` IfObjective ((>= -tolerance) . maximum)
      let solver = solverReal problem 101 11 sbx stop
      (pop, dist) <- runSolverReal problem solver
      let best = head $ bestFirst Minimizing pop
      let bestG = takeGenome best
      let bestF = takeObjectiveValue  best
      pr ""
      pr $ "best:    " ++ (intercalate " " (map (printf "%.5f") bestG))
      pr $ "residual: " ++ (printf "%.5g" bestF)
      assertBool ("residual < " ++ show (negate tolerance)) (bestF >= -tolerance)
  , "Rosenbrock 2D GM/BLX-0.5/min residual, max 500 gens" ~: do
      let tolerance = 1e-3  -- solution error
      let maxiters = 500
      let problem = RealMinimize rosenbrock [(-20,20),(-20,20)] [1,1]
      let stop = Generations maxiters
      let solver = solverReal problem 400 11 blxa stop
      (pop, dist) <- runSolverReal problem solver
      let bestG = takeGenome . head $ bestFirst Minimizing pop
      pr ""
      pr $ "best:    " ++ (intercalate " " (map (printf "%.5f") bestG))
      pr $ "error:   " ++ (printf "%.5g" dist)
      assertBool ("error = " ++ show dist ++ " >= " ++ show tolerance) (dist < tolerance)
  , "Rosenbrock 2D GM/UNDX/GensNoChange 10" ~: do
      let maxiters = 5000
      let popsize = 101
      let elite = 11
      let nochange = 10
      let select = tournamentSelect Minimizing 3 (popsize - elite)
      let stop = (GensNoChange nochange (round.(*1e3).maximum) Nothing) `Or` (Generations maxiters)
      let step = nextGeneration Minimizing rosenbrock select elite undx (gauss 1.0 2)
      let log = WriteEvery 1 (\_ p -> [minimum . map takeObjectiveValue $ p])
      let ga = loopWithLog log stop step
      let init = replicateM popsize . replicateM 2 $ getRandomR (-10,10)

      (pop, hist) <- runGA init ga

      let best = takeGenome . head $ bestFirst Minimizing pop
      pr ""
      pr $ "best:    " ++ (intercalate " " (map (printf "%.5f") best))
      let lastbest = take nochange (reverse hist)
      pr $ "last best: "
      mapM_ pr (map show $ reverse lastbest)
      assertBool "false positive on GensNoChange"
                     (all id $ zipWith (==) lastbest (drop 1 lastbest))
  ]
