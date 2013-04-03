Moo
===

Genetic algorithm library for Haskell.


Features
--------

  * Binary genetic algorithms:

    - binary and Gray encoding
    - one-point, two-point, and uniform crossover
    - point, asymmetric and constant-frequency mutation

  * Continous (real-valued) genetic algorithms:

    - BLX-α (blend), UNDX (unimodal normal distributed), SBX (simulated binary) crossover
    - one-point, two-point, and uniform crossover
    - Gaussian mutation

  * Selection:

    - minimization and maximization problems
    - roulette
    - tournament
    - optional ranking and scaling
    - optional elitism

  * Iteration control

    - stop conditions:

      + number of generations
      + values of objective function
      + stall of objective function
      + custom and interactive (`loopIO`)
      + time limit (`loopIO`)
      + compound conditions (`And`, `Or`)

    - optional logging (either pure, or in `IO`)

  * Initialization

    - random uniform
    - arbitrary custom


An example
----------

Minimizing [Beale's function][test-functions]:

```haskell
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
initialize = getRandomGenomes popsize 2 (-4.5, 4.5)


main = do
  population <- runGA initialize (loop stop step)
  print (head . bestFirst Minimizing $ population)
```

For more examples, see [examples/](examples/) folder.

[test-functions]: http://en.wikipedia.org/wiki/Test_functions_for_optimization
