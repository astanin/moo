{- Minimize Rosenbrock function using real-valued genetic algorithm.
   Optimal value x* = (1,...,1). F(x*) = 0.
-}

import AI.SimpleEA
import AI.SimpleEA.Utils
import AI.SimpleEA.Rand
import Control.Monad
import Data.List
import Data.Ord (comparing)
import Print (printHistoryAndBest)
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

-- digest: what to log on every iteration
digest pop =
  let m = maxFitness pop
      a = avgFitness pop
  in  (a, m)

geneticAlgorithm mutate crossover = do
  -- initial population
  genomes0 <- replicateM popsize $ replicateM nvariables (getRandomR xrange)
  let pop0 = evalFitness fitness genomes0
  -- run genetic algorithm
  loopUntil' (MaxFitness (>= (-precision))
              `Or` Iteration maxiters) digest pop0 $
              nextGeneration elitesize fitness select crossover mutate

-- usage: rosenbrock mutationOperator crossoverOperator
main = do
  args <- getArgs
  conf <- case args of
           (m:x:[]) -> return (lookup m mutationOps, lookup x crossoverOps)
           _        -> printUsage
  case conf of
    (Just mutate, Just crossover) -> do
       (pop, log) <- runGA $ geneticAlgorithm mutate crossover
       printHistoryAndBest show pop log
       let bestF = snd . head . sortBy (comparing (snd)) $ pop
       if (bestF >= (-precision))
          then exitWith ExitSuccess
          else exitWith (ExitFailure 2)  -- failed to find a solution
    _ -> printUsage
