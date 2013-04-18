{- Schaffer function #2. Minimium at (0,0), equal to 0. -}

import Moo.GeneticAlgorithm.Continuous
import Moo.GeneticAlgorithm.Constraints


import Data.Function (on)


import ExampleMain


schafferN2 :: [Double] -> Double
schafferN2 [x,y] = 0.5 + (sin(x*x-y*y)**2 - 0.5)/(1+0.001*(x*x+y*y))**2
xvar [x,_] = x
yvar [_,y] = y


constraints = [ (-100.0) .<= xvar <=. 100.0
              , (-100.0) .<= yvar <=. 100.0 ]


popsize = 100
initialize = getRandomGenomesRs popsize (replicate 2 (-512,512))
select = withConstraints constraints (degreeOfViolation 1 0) Minimizing $
         withPopulationTransform (fitnessSharing dist 1.0 1 Minimizing) $
         tournamentSelect Minimizing 2 popsize
crossover = unimodalCrossoverRP
mutate = gaussianMutate 0.05 0.1
step = nextSteadyState (popsize `div` 100) Minimizing schafferN2
       select crossover mutate

dist = distGenotypes `on` takeGenome
    where
      distGenotypes [x1,y1] [x2,y2] = sqrt ((x1-x2)**2 + (y1-y2)**2)



main = exampleMain (exampleDefaults { numGenerations = 1000 })
       Minimizing initialize step
