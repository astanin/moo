module Main where

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Multiobjective


import System.Environment (getArgs)


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
  putStrLn $ show n ++ " genomes"
  result <- runGA
            (return . take n . cycle $ genomes) $
            (loop (Generations 100) step)
  putStrLn $ "best:"
  mapM_ print (mape fst . take 10 $ result)