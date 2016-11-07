{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- |
Copyright    : 2010-2011 Erlend Hamberg, 2011-2013 Sergey Astanin
License      : BSD3
Stability    : experimental
Portability  : portable

A library for custom genetic algorithms.

@
-----------
Quick Start
-----------
@

Import

  * either "Moo.GeneticAlgorithm.Binary"

  * or "Moo.GeneticAlgorithm.Continuous"

Genetic algorithms are used to find good solutions to optimization
and search problems. They mimic the process of natural evolution
and selection.

A genetic algorithm deals with a /population/ of candidate solutions.
Each candidate solution is represented with a 'Genome'. On every
iteration the best genomes are /selected/ ('SelectionOp'). The next
generation is produced through /crossover/ (recombination of the
parents, 'CrossoverOp') and /mutation/ (a random change in the genome,
'MutationOp') of the selected genomes. This process of selection --
crossover -- mutation is repeated until a good solution appears or all
hope is lost.

Genetic algorithms are often defined in terms of minimizing a cost
function or maximizing fitness. This library refers to observed
performance of a genome as 'Objective', which can be minimized as well
as maximized.


@
--------------------------------
How to write a genetic algorithm
--------------------------------
@

  1. Provide an encoding and decoding functions to convert from model
     variables to genomes and back. See /How to choose encoding/ below.

  2. Write a custom objective function. Its type should be an instance
     of 'ObjectiveFunction' @a@. Functions of type @Genome a -> Objective@
     are commonly used.

  3. Optionally write custom selection ('SelectionOp'), crossover
     ('CrossoverOp') and mutation ('MutationOp') operators or just use
     some standard operators provided by this library. Operators specific
     to binary or continuous algorithms are provided by
     "Moo.GeneticAlgorithm.Binary" and "Moo.GeneticAlgorithm.Continuous"
     modules respectively.

  4. Use 'nextGeneration' or 'nextSteadyState' to create a single step
     of the algorithm, control the iterative process with 'loop',
     'loopWithLog', or 'loopIO'.

  5. Write a function to generate an initial population; for random
     uniform initialization use 'getRandomGenomes'
     or 'getRandomBinaryGenomes'.

Library functions which need access to random number generator work in
'Rand' monad.  You may use a high-level wrapper 'runGA' (or
'runIO' if you used 'loopIO'), which takes care of creating a new random
number generator and running the entire algorithm.

To solve constrained optimization problems, modify initialization and
selection operators (see "Moo.GeneticAlgorithm.Constraints").

To solve multi-objective optimization problems, use NSGA-II algorithm
(see "Moo.GeneticAlgorithm.Multiobjective").

@
----------------------
How to choose encoding
----------------------
@

 * For problems with discrete search space, binary (or Gray)
   encoding of the bit-string is usually used.
   A bit-string is represented as a list of @Bool@ values (@[Bool]@).
   To build a binary genetic algorithm, import "Moo.GeneticAlgorithm.Binary".

 * For problems with continuous search space, it is possible to use a
   vector of real variables as a genome.
   Such a genome is represented as a list of @Double@ or @Float@ values.
   Special crossover and mutation operators should be used.
   To build a continuous genetic algorithm, import
   "Moo.GeneticAlgorithm.Continuous".


@
--------
Examples
--------
@

Minimizing Beale's function:

@
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
stop = IfObjective (\\values -> (minimum values) < tolerance)
initialize = getRandomGenomes popsize [(-4.5, 4.5), (-4.5, 4.5)]


main = do
  population <- runGA initialize (loop stop step)
  print (head . bestFirst Minimizing $ population)
@

See @examples/@ folder of the source distribution for more examples.

-}

module Moo.GeneticAlgorithm (
) where

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Run
import Moo.GeneticAlgorithm.Utilities
import Moo.GeneticAlgorithm.Binary
import Moo.GeneticAlgorithm.Continuous
