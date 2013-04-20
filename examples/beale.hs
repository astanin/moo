{- Minimize Beale function using real-valued genetic algorithm.
   Optimal value x* = [3, 0.5]. F(x*) = 0.
-}

import Moo.GeneticAlgorithm.Continuous


beale :: [Double] -> Double
beale [x, y] = (1.5 - x + x*y)**2 + (2.25 - x + x*y*y)**2 + (2.625 - x + x*y*y*y)**2


popsize = 101
elitesize = 1
tolerance = 1e-6


selection = tournamentSelect Minimizing 2 (popsize - elitesize)
crossover = unimodalCrossoverRP
mutation = gaussianMutate 0.25 0.1
step = nextGeneration Minimizing beale selection elitesize crossover mutation
stop = IfObjective (\values -> (minimum values) < tolerance)
initialize = getRandomGenomes popsize [(-4.5, 4.5), (-4.5, 4.5)]


main = do
  population <- runGA initialize (loop stop step)
  print (head . bestFirst Minimizing $ population)