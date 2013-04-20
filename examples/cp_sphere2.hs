{- Constrained problem

   min (x^2 + y^2)

   with x + y >= 1.

-}

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints


import ExampleMain


f :: [Double] -> Double
f [x, y] = x*x + y*y


constraints = [ sum .>=. 1 ]


popsize = 100


initialize = getRandomGenomes popsize [(-10,10),(-5,5)]
select = tournamentSelect Minimizing 2 popsize
crossover = unimodalCrossoverRP
mutation = noMutation


step = withDeathPenalty constraints $
       nextGeneration Minimizing f select 2 crossover mutation


{-
-- exampleMain takes care of command line options and pretty printing.
-- If you don't need that, a bare bones main function looks like this:

main = do
  results <- runGA initialize (loop (Generations 25) step)
  print . head . bestFirst Minimizing $ results

-}
main = exampleMain (exampleDefaults { numGenerations = 25 })
       Minimizing initialize step
