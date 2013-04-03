{- Minimize Rosenbrock function using real-valued genetic algorithm.
   Optimal value x* = (1,...,1). F(x*) = 0.

   It is a real-values genetic algorithm. The user may choose a
   mutation and crossover operators.  This example uses hooks to save
   evolution history.

   To run:

       ghc --make rosenbrock.hs
       ./rosenbrock gm undx > output.txt

   To visualize the output in gnuplot:

       % gnuplot
       > set logscale y ; set xlabel 'generation' ;
       > plot 'output.txt' u 1:2 w l t 'median', '' u 1:3 w l t 'best' lt 3


-}

import Moo.GeneticAlgorithm.Continuous

import Control.Monad
import Data.List
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Printf (printf)

rosenbrock :: [Double] -> Double
rosenbrock xs = sum . map f $ zip xs (drop 1 xs)
  where
   f (x1, x2) = 100.0 * (x2 - x1^(2::Int))^(2::Int) + (x1 - 1)^(2::Int)

nvariables = 3
xrange = (-30.0, 30.0)
popsize = 100
precision = 1e-5
maxiters = 4000 :: Int
elitesize = 10

-- Rosenbrock function is minimized
objective :: [Double] -> Objective
objective xs = rosenbrock xs

-- selection: tournament selection
select = tournamentSelect Minimizing 3 (popsize-elitesize)

-- Gaussian mutation, mutate fraction @genomeschanged@ of the population
gm genomeschanged =
    let p = 1.0 - (1.0 - genomeschanged)**(1.0 / fromIntegral nvariables)
        s = 0.01*(snd xrange - fst xrange)
    in  gaussianMutate p s

mutationOps = [ ("gm", gm 0.33) ]

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

logStats = WriteEvery 10 $ \iterno pop ->
             let pop' =  bestFirst Minimizing pop
                 bestobjval = takeObjectiveValue $ head pop'
                 medianobjval = takeObjectiveValue $ pop' !! (length pop' `div` 2)
             in  [(iterno, medianobjval, bestobjval)]

printStats :: [(Int, Objective, Objective)] -> IO ()
printStats stats = do
  printf "# %-10s %15s %15s\n" "generation" "median" "best"
  flip mapM_ stats $ \(iterno, median, best) ->
      printf "%12d %15.3g %15.3g\n" iterno median best

geneticAlgorithm mutate crossover = do
  -- initial population
  let initialize = replicateM popsize $ replicateM nvariables (getRandomR xrange)
  let stop = IfObjective ((<= precision) . minimum) `Or` Generations maxiters
  let step = nextGeneration Minimizing objective select elitesize crossover mutate
  --
  let ga = loopWithLog logStats stop step
  runGA initialize ga


printBest :: Population Double -> IO ()
printBest pop = do
  let bestGenome = takeGenome . head $ bestFirst Minimizing pop
  let vals = map (\x -> printf "%.5f" x) bestGenome
  putStrLn $ "# best solution: " ++ (intercalate ", " vals)

-- usage: rosenbrock mutationOperator crossoverOperator
main = do
  args <- getArgs
  conf <- case args of
           []       -> return (lookup "gm" mutationOps, lookup "undx" crossoverOps)
           (m:x:[]) -> return (lookup m mutationOps, lookup x crossoverOps)
           _        -> printUsage
  case conf of
    (Just mutate, Just crossover) -> do
       (pop, stats) <- geneticAlgorithm mutate crossover
       printStats stats
       printBest pop
       -- exit status depends on convergence
       let bestF = takeObjectiveValue . head $ bestFirst Minimizing pop
       if (abs bestF <= precision)
          then exitWith ExitSuccess
          else do
            printf "# failed to converge: best residual=%.5g, target=%g\n" bestF precision
            exitWith (ExitFailure 2)  -- failed to find a solution
    _ -> printUsage
