{- Minimize Rosenbrock function using real-valued genetic algorithm.
   Optimal value x* = (1,...,1). F(x*) = 0.
-}

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Utilities
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Run
import Control.Monad
import Data.List
import Data.Ord (comparing)
import Print (printBest)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))

rosenbrock :: [Double] -> Double
rosenbrock xs = sum . map f $ zip xs (drop 1 xs)
  where
   f (x1, x2) = 100 * (x2 - x1^2)^2 + (x1 - 1)^2

nvariables = 2
xrange = (-30.0, 30.0)
popsize = 100
precision = 1e-4
maxiters = 1000 :: Int
elitesize = 2

-- fitness function is maximized  when Rosenbrock function is minimized
fitness xs _ = negate $ rosenbrock xs

-- selection: tournament selection
select = tournamentSelect 3 (popsize-10)

-- Gaussian mutation
mutate =
    let p = 0.5/fromIntegral nvariables
        s = 0.01*(snd xrange - fst xrange)
    in  gaussianMutate p s

mutationOps = [ ("gm", mutate) ]

-- BLX-0.5 crossover
blxa = blendCrossover 0.5
-- UNDX crossover
undx = unimodalCrossoverRP
-- SBX crossover
sbx = simulatedBinaryCrossover 2

crossoverOps = [ ("blxa", blxa), ("undx", undx), ("sbx", sbx) ]

printUsage = do
  putStrLn usage
  exitWith (ExitFailure 1)
  where
  usage = intercalate " " [ "rosenbrock", mops, xops ]
  mops = intercalate "|" (map fst mutationOps)
  xops = intercalate "|" (map fst crossoverOps)

geneticAlgorithm mutate crossover = do
  -- initial population
  genomes0 <- replicateM popsize $ replicateM nvariables (getRandomR xrange)
  let pop0 = evalFitness fitness genomes0
  let step = nextGeneration elitesize fitness select crossover mutate
  -- run genetic algorithm
  loopUntil (MaxFitness (>= (-precision)) `Or` Iteration maxiters) pop0 step


-- usage: rosenbrock mutationOperator crossoverOperator
main = do
  args <- getArgs
  conf <- case args of
           (m:x:[]) -> return (lookup m mutationOps, lookup x crossoverOps)
           _        -> printUsage
  case conf of
    (Just mutate, Just crossover) -> do
       pop <- runGA $ geneticAlgorithm mutate crossover
       printBest show pop
       let bestF = snd . head . sortBy (comparing (snd)) $ pop
       if (bestF >= (-precision))
          then exitWith ExitSuccess
          else exitWith (ExitFailure 2)  -- failed to find a solution
    _ -> printUsage
