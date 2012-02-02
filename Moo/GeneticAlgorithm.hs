{-# OPTIONS_GHC -fno-warn-unused-imports #-}

{- |
Copyright    : 2010-2011 Erlend Hamberg, 2011-2012 Sergey Astanin
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
/genome/. On every iteration the most fittest genomes are
/selected/. The next generation is produced through /crossover/
(recombination of the parents) and /mutation/ (a random change in the
genome) of the selected genomes. This process of selection --
crossover -- mutation is repeated until a good enough solution appears
or all hope is lost.

Sometimes genetic algorithms are defined in terms of minimizing
a cost function rather than maximizing fitness.

/How to write a genetic algorithm/

  1. Provide an encoding and decoding functions to convert from model
     variables to genomes and back. See /How to choose encoding/ below.

  2. Write a custom fitness function to maximize. Its type should be
     'FitnessFunction' @a@.

  3. Optionally write custom selection ('SelectionOp'), crossover
     ('CrossoverOp') and mutation ('MutationOp') operators or just use
     some standard operators provided by this library.

  4. Generate an initial population randomly, encode it to genomes,
     evaluate fitness ('evalFitness'). "Moo.GeneticAlgorithm.Random"
     provides necessary functions to generate random variables, but
     'getRandomGenomes' is usually sufficient.

  5. Use 'nextGeneration' and 'loopUntil' or 'loopUntil'' to repeat
     the iterations as long as necessary.

Library functions which need access to random number generator work in
'Rand' monad. Wrap such functions with 'runGA' or 'runRandom'.

/How to choose encoding/

 * For problems with discrete variables, binary (or Gray)
   encoding of the bit-string is usually used.
   You can represent bit-string as a list of @Bool@ values (@[Bool]@).
   To build a binary genetic algorithm, import "Moo.GeneticAlgorithm.Binary".

 * For continuous (real-valued) problems, it is possible to use a
   vector or real-valued variables as a genome.
   You can represent such genome as a list of @Double@ or @Float@ values.
   Special crossover and mutation operators should be used.
   To build a continuous genetic algorithm, import
   "Moo.GeneticAlgorithm.Continuous".

/Other modules/

"Moo.GeneticAlgorithm.Crossover": crossover operations.
Mostly intended for binary GAs.

"Moo.GeneticAlgorithm.Selection": several selection operators.

"Moo.GeneticAlgorithm.Random": facilities related to random number
generation.

"Moo.GeneticAlgorithm.Run": bring everything together and actually run
the algorithm.

"Moo.GeneticAlgorithm.Types": types for data structures and operatores.

"Moo.GeneticAlgorithm.Utils": common non-deterministic and statistic functions.

/Examples/

See @examples/@ folder of the source distribution.

-}

module Moo.GeneticAlgorithm (
) where

import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Run
import Moo.GeneticAlgorithm.Utilities
