{- Minimize Beale function using real-valued genetic algorithm.
   Optimal value x* = [3, 0.5]. F(x*) = 0.
-}

import Moo.GeneticAlgorithm.Continuous
import Control.Monad (replicateM)


beale :: [Double] -> Double
beale [x, y] = (1.5 - x + x*y)**2 + (2.25 - x + x*y*y)**2 + (2.625 - x + x*y*y*y)**2


popsize = 101
elitesize = 1
tolerance = 1e-6
iterations = 2000


selection = tournamentSelect Minimizing 2 (popsize - elitesize)
crossover = unimodalCrossoverRP
mutation = gaussianMutate 0.25 0.1
step = nextGeneration Minimizing beale selection elitesize crossover mutation
stop = IfObjective (\vs -> (minimum vs) < tolerance) `Or` Generations iterations
initialize = replicateM popsize $ replicateM 2 (getRandomR (-4.5, 4.5))


main = do
  r <- runGA initialize (loop stop step)
  print (head . bestFirst Minimizing $ r)