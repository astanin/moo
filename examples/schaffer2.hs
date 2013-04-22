{- Schaffer function #2. Minimium at (0,0), equal to 0. -}

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints


import Data.Function (on)


import ExampleMain


schafferN2 :: [Double] -> Double
schafferN2 [x,y] = 0.5 + (sin(x*x-y*y)**2 - 0.5)/(1+0.001*(x*x+y*y))**2
xvar [x,_] = x
yvar [_,y] = y


popsize = 100
initialize = getRandomGenomes popsize (replicate 2 (-100,100))
select = withFitnessSharing (distance2 `on` takeGenome) 1.0 1 Minimizing $
         tournamentSelect Minimizing 2 popsize
crossover = unimodalCrossoverRP
mutate = gaussianMutate 0.05 0.1
step = nextSteadyState (popsize `div` 100) Minimizing schafferN2
       select crossover mutate


{-
-- exampleMain takes care of command line options and pretty printing.
-- If you don't need that, a bare bones main function looks like this:

main = do
  results <- runGA initialize (loop (Generations 1000) step)
  print . head . bestFirst Minimizing $ results

-}
main = exampleMain (exampleDefaults { numGenerations = 1000 })
       Minimizing initialize step
