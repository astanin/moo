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
       > plot 'output.txt' u 1:(-$2) w l t 'median', '' u 1:(-$3) w l t 'best' lt 3


-}

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Utilities
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Run
import Control.Monad
import Data.List
import Data.Ord (comparing)
import System.Environment (getArgs)
import System.Exit (exitWith, ExitCode(..))
import Text.Printf (printf)

rosenbrock :: [Double] -> Double
rosenbrock xs = sum . map f $ zip xs (drop 1 xs)
  where
   f (x1, x2) = 100 * (x2 - x1^2)^2 + (x1 - 1)^2

nvariables = 3
xrange = (-30.0, 30.0)
popsize = 100
precision = 1e-5
maxiters = 2000 :: Int
elitesize = 10

-- fitness function is maximized  when Rosenbrock function is minimized
fitness xs _ = negate $ rosenbrock xs

-- selection: tournament selection
select = tournamentSelect 3 (popsize-elitesize)

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

logStats = WriteEvery 10 $ \iterno pop ->
             let pop' =  sortByFitness pop
                 bestfitness = snd $ head pop'
                 medianfitness = snd $ pop' !! (length pop' `div` 2)
             in  [(iterno, medianfitness, bestfitness)]

printStats :: [(Int, Fitness, Fitness)] -> IO ()
printStats stats = do
  printf "# %-10s %15s %15s\n" "generation" "medianFitness" "bestFitness"
  flip mapM_ stats $ \(iterno, median, best) ->
      printf "%12d %15.3g %15.3g\n" iterno median best

geneticAlgorithm mutate crossover = do
  -- initial population
  genomes0 <- replicateM popsize $ replicateM nvariables (getRandomR xrange)
  let pop0 = evalFitness fitness genomes0
  let step = nextGeneration elitesize fitness select crossover mutate
  -- run genetic algorithm
  let stopcond = IfFitness ((>= -precision) . maximum) `Or` Iteration maxiters
  loopUntilWithHooks [logStats] stopcond step pop0


printBest :: Population Double -> IO ()
printBest pop = do
  let bestGenome = takeGenome . head $ sortByFitness pop
  let vals = map (\x -> printf "%.5f" x) bestGenome
  putStrLn $ "# best solution: " ++ (intercalate ", " vals)

-- usage: rosenbrock mutationOperator crossoverOperator
main = do
  args <- getArgs
  conf <- case args of
           (m:x:[]) -> return (lookup m mutationOps, lookup x crossoverOps)
           _        -> printUsage
  case conf of
    (Just mutate, Just crossover) -> do
       (pop, stats) <- runGA $ geneticAlgorithm mutate crossover
       printStats stats
       printBest pop
       -- exit status depends on convergence
       let bestF = takeFitness . head $ sortByFitness pop
       if (abs bestF <= precision)
          then exitWith ExitSuccess
          else do
            printf "# failed to converge: best fitness=%.5f, target=-%g\n" bestF precision
            exitWith (ExitFailure 2)  -- failed to find a solution
    _ -> printUsage
