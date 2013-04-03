{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- |
Copyright    : 2010-2011 Erlend Hamberg, 2011-2013 Sergey Astanin
License      : BSD3
Stability    : experimental
Portability  : portable

A library for custom genetic algorithms.

Quick start:

  * "Moo.GeneticAlgorithm.Types"

  * either "Moo.GeneticAlgorithm.Binary" or "Moo.GeneticAlgorithm.Continuous"

  * "Moo.GeneticAlgorithm.Run"

Genetic algorithms are used to find good solutions to optimization
and search problems. They mimic the process of natural evolution
and selection.

A genetic algorithm deals with a /population/ of candidate
solutions.  Each candidate solution is represented with a
/genome/. On every iteration the best genomes are
/selected/. The next generation is produced through /crossover/
(recombination of the parents) and /mutation/ (a random change in the
genome) of the selected genomes. This process of selection --
crossover -- mutation is repeated until a good enough solution appears
or all hope is lost.

Genetic algorithms are often defined in terms of minimizing a cost
function or maximizing fitness. This library refers to observed
performance of a genome as 'Objective', which can be minimized as well
as maximized.

/How to write a genetic algorithm/

  1. Provide an encoding and decoding functions to convert from model
     variables to genomes and back. See /How to choose encoding/ below.

  2. Write a custom objective function. Its type should be
     'ObjectiveFunction' @a@.

  3. Optionally write custom selection ('SelectionOp'), crossover
     ('CrossoverOp') and mutation ('MutationOp') operators or just use
     some standard operators provided by this library.

  4. Use 'nextGeneration' to create a single step of the algorithm,
     control the iterative process with 'loop', 'loopWithLog', or 'loopIO'.

  5. Write a function to generate an initial population.
     "Moo.GeneticAlgorithm.Random" provides necessary functions to
     generate random variables.

Library functions which need access to random number generator work in
'Rand' monad.  You may use a high-level wrapper 'runGA' (or
'runIO' if you used 'loopIO'), which takes care of creating a new random
number generator and running the entire algorithm.


/How to choose encoding/

 * For problems with discrete search space, binary (or Gray)
   encoding of the bit-string is usually used.
   A bit-string is represtented as a list of @Bool@ values (@[Bool]@).
   To build a binary genetic algorithm, import "Moo.GeneticAlgorithm.Binary".

 * For problems with continuous search space, it is possible to use a
   vector of real-valued variables as a genome.
   Such a genome is represented as a list of @Double@ or @Float@ values.
   Special crossover and mutation operators should be used.
   To build a continuous genetic algorithm, import
   "Moo.GeneticAlgorithm.Continuous".

/Other modules/

"Moo.GeneticAlgorithm.Types":
  types for data structures and operators.

"Moo.GeneticAlgorithm.Crossover":
  crossover operations. Mostly intended for binary GAs.

"Moo.GeneticAlgorithm.Selection":
  several selection operators.

"Moo.GeneticAlgorithm.Random":
  facilities related to random number generation.

"Moo.GeneticAlgorithm.Run":
   bring everything together and actually run the algorithm.

"Moo.GeneticAlgorithm.Utilities", "Moo.GeneticAlgorithm.Statistics":
  common non-deterministic and statistic functions.

/Examples/

See @examples/@ folder of the source distribution.

-}

module Moo.GeneticAlgorithm (
) where

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Run
import Moo.GeneticAlgorithm.Utilities
