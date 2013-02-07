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
  , "Rosenbrock 2D GM/UNDX/GensNoChange 10" ~: do
      let maxiters = 2000
      let popsize = 101
      let elite = 11
      let nochange = 10
      let fitness xs _ = rosenbrock $ xs
      let select = minimizing $ tournamentSelect 3 (popsize - elite)
      let stop = (GensNoChange nochange (round.(*1e3).maximum) Nothing) `Or` (Iteration maxiters)
      let step = nextGeneration elite fitness select undx (gauss 1.0 2)
      let log = WriteEvery 1 (\_ p -> [maximum . map takeFitness $ p])
      let ga = loopUntilWithHooks [log] stop step

      rng <- newPureMT
      let (pop, hist) = flip evalRandom rng $ do
                          gens0 <- replicateM popsize . replicateM 2 $ getRandomR (-10,10)
                          let pop0 = evalFitness fitness gens0
                          ga pop0

      let best = takeGenome . head $ sortByCost pop
      pr ""
      pr $ "best:    " ++ (intercalate " " (map (printf "%.5f") best))
      let lastbest = take nochange (reverse hist)
      pr $ "last best: "
      mapM_ pr (map show $ reverse lastbest)
      assertBool "false positive on GensNoChange"
                     (all id $ zipWith (==) lastbest (drop 1 lastbest))
  ]
