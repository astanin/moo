Moo
===

     ------------------------------------------------
    < Moo. Breeding Genetic Algorithms with Haskell. >
     ------------------------------------------------
            \   ^__^
             \  (oo)\_______
                (__)\       )\/\
                    ||----w |
                    ||     ||


Installation
------------

  * install Haskell Platform or GHC and cabal-install
  * run `cabal update`
  * run `cabal install moo`


Build status: [![Build Status](https://drone.io/github.com/astanin/moo/status.png)](https://drone.io/github.com/astanin/moo/latest)


Features
--------

    |                       | Binary GA            | Continuous GA            |
    |-----------------------+----------------------+--------------------------|
    |Encoding               | binary bit-string    | sequence of real values  |
    |                       | Gray bit-string      |                          |
    |-----------------------+----------------------+--------------------------|
    |Initialization         |            random uniform                       |
    |                       |            constrained random uniform           |
    |                       |            arbitrary custom                     |
    |-----------------------+-------------------------------------------------|
    |Objective              |            minimization and maximiation         |
    |                       |            optional scaling                     |
    |                       |            optional ranking                     |
    |                       |            optional niching (fitness sharing)   |
    |-----------------------+-------------------------------------------------|
    |Selection              |            roulette                             |
    |                       |            stochastic universal sampling        |
    |                       |            tournament                           |
    |                       |            optional elitism                     |
    |                       |            optionally constrained               |
    |                       |            custom non-adaptive ^                |
    |-----------------------+-------------------------------------------------|
    |Crossover              |            one-point                            |
    |                       |            two-point                            |
    |                       |            uniform                              |
    |                       |            custom non-adaptive ^                |
    |                       +----------------------+--------------------------|
    |                       |                      | BLX-α (blend)            |
    |                       |                      | SBX (simulated binary)   |
    |                       |                      | UNDX (unimodal normally  |
    |                       |                      | distributed)             |
    |-----------------------+----------------------+--------------------------|
    |Mutation               | point                | Gaussian                 |
    |                       | asymmetric           |                          |
    |                       | constant frequency   |                          |
    |                       +----------------------+--------------------------|
    |                       |            custom non-adaptive ^                |
    |-----------------------+-------------------------------------------------|
    |Replacement            |            generational with elitism            |
    |                       |            steady state                         |
    |-----------------------+-------------------------------------------------|
    |Stop                   |            number of generations                |
    |condition              |            values of objective function         |
    |                       |            stall of objective function          |
    |                       |            custom or interactive (`loopIO`)     |
    |                       |            time limit (`loopIO`)                |
    |                       |            compound conditions (`And`, `Or`)    |
    |-----------------------+-------------------------------------------------|
    |Logging                |            pure periodic (any monoid)           |
    |                       |            periodic with `IO`                   |
    |-----------------------+-------------------------------------------------|
    |Constrainted           |            constrained initialization           |
    |optimization           |            constrained selection                |
    |                       |            death penalty                        |
    |-----------------------+-------------------------------------------------|
    |Multiobjective         |            NSGA-II                              |
    |optimization           |            constrained NSGA-II                  |


`^` non-adaptive: any function which doesn't depend on generation number

There are other possible encodings which are possible to represent
with list-like genomes (`type Genome a = [a]`):

  * permutation encodings (`a` being an integer, or other `Enum` type)
  * tree encodings (`a` being a subtree type)
  * hybrid encodings (`a` being a sum type)


Contributing
------------

There are many ways you can help developing the library:

  * I'm not a native speaker of English. If you are, please proof-read
    and correct the comments and the documentation.

  * Moo is designed with possibility of implementing custom genetic
    operators in mind. If you write new operators (`SelectionOp`,
    `CrossoverOp`, `MutationOp`) or replacement strategies
    (`StepGA`), consider contributing them to the library.
    In the comments please give a reference to an academic
    work which introduces or studies the method. Explain when or why
    it should be used. Provide tests and examples if possible.

  * Implementing some methods (like adaptive genetic algorithms) will
    require to change some library types. Please discuss your approach
    first.

  * Contribute examples. Solutions of known problems with known optima
    and interesting properties. Try to avoid examples which are too
    contrived.



An example
----------

Minimizing [Beale's function][test-functions] (optimal value f(3, 0.5) = 0):

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
initialize = getRandomGenomes popsize [(-4.5, 4.5), (-4.5, 4.5)]


main = do
  population <- runGA initialize (loop stop step)
  print (head . bestFirst Minimizing $ population)
```

For more examples, see [examples/](examples/) folder.

[test-functions]: http://en.wikipedia.org/wiki/Test_functions_for_optimization
