module Main where

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Multiobjective


import System.Environment (getArgs)
import Text.Printf (printf)


mop :: MultiObjectiveProblem ([Double] -> Double)
mop = [ (Minimizing, sum :: [Double] -> Double)
      , (Maximizing, product)]


genomes :: [[Double]]
genomes = [[3,3], [9,1], [1,4], [2,2], [1,9], [4,1], [1,1], [4,2]]


step :: StepGA Rand Double
step = stepNSGA2default mop noCrossover (gaussianMutate 0.5 0.5)


main = do
  args <- getArgs
  let args' = zip (iterate (+ 1) 0) args
  let n = maybe (length genomes) read $ lookup (0::Int) args'
  putStrLn $ "# population size: " ++ show n
  result <- runGA
            (return . take n . cycle $ genomes) $
            (loop (Generations 100) step)
  putStrLn $ "# best:"
  printPareto result


printPareto result = do
  let paretoGenomes = map takeGenome . takeWhile ((== 1.0) . takeObjectiveValue) $ result
  let paretoObjectives = map takeObjectiveValues $ evalAllObjectives mop paretoGenomes
  putStr $ unlines $
       map (\[x,y] -> printf "%12.3f\t%12.3f" x y ) paretoObjectives