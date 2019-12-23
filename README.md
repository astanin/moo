Moo
===

```

     ------------------------------------------------
    < Moo. Breeding Genetic Algorithms with Haskell. >
     ------------------------------------------------
            \   ^__^
             \  (oo)\_______
                (__)\       )\/\
                    ||----w |
                    ||     ||

```

Installation
------------

### Installation from Hackage

Hackage is a Haskell community's package archive. This is where the
latest versions of packages are published first. To install Moo from
Hackage use Cabal-Install:

-   install Haskell Platform or GHC and Cabal-Install
-   run `cabal update`
-   run `cabal install moo`

### Installation with Stack

Stackage is a stable package archive. Stackage builds are supposed to be
reproducible. Stackage also provides Long Term Support releases. To
build Moo with Stackage dependencies, use the \`stack\` tool:

-   install [`stack`](https://docs.haskellstack.org/)
-   if necessary, install GHC by running `stack setup`
-   run `stack update`
-   in the project source directory run `stack build`
-   to run tests `stack test`

### Build Status

[![Build Status](https://travis-ci.org/astanin/moo.svg?branch=master)](https://travis-ci.org/astanin/moo)

Features
--------

<table class="tg">
  <tr>
    <th class="tg-9wq8"><span style="font-weight:bold">Role</span></th>
    <th class="tg-9wq8"><span style="font-weight:bold">Binary parameters</span></th>
    <th class="tg-9wq8"><span style="font-weight:bold">Continuous parameters</span></th>
  </tr>
  <tr>
    <td class="tg-9wq8">Encoding</td>
    <td class="tg-9wq8"><br>binary bit-string<br>Gray bit-string<br></td>
    <td class="tg-9wq8">sequence of real values</td>
  </tr>
  <tr>
    <td class="tg-9wq8">Initialisation</td>
    <td class="tg-9wq8" colspan="2"><br>random uniform<br>constrained random uniform<br>arbitrary custom<br></td>
  </tr>
  <tr>
    <td class="tg-baqh">Objective</td>
    <td class="tg-baqh"><br>minimisation and maximisation<br>optional scaling<br>optional ranking<br>optional niching (fitness sharing)<br></td>
    <td class="tg-baqh"></td>
  </tr>
  <tr>
    <td class="tg-baqh">Selection</td>
    <td class="tg-baqh" colspan="2"><br>roulette<br>stochastic universal sampling<br>tournament<br>optional elitism<br>optionally constrained<br>custom non-adaptive ^<br></td>
  </tr>
  <tr>
    <td class="tg-baqh" rowspan="2">Crossover</td>
    <td class="tg-baqh" colspan="2"><br>one-point<br>two-point<br>uniform<br>custom non-adaptive ^<br></td>
  </tr>
  <tr>
    <td class="tg-baqh"></td>
    <td class="tg-baqh"><br>BLX-alpha (blend)<br>SBX (simulated binary)<br>UNDX (unimodal normally distributed)<br></td>
  </tr>
  <tr>
    <td class="tg-baqh" rowspan="2">Mutation</td>
    <td class="tg-baqh"><br>point<br>asymmetric<br>constant frequency<br></td>
    <td class="tg-baqh">Gaussian</td>
  </tr>
  <tr>
    <td class="tg-baqh" colspan="2">custom non-adaptive ^</td>
  </tr>
  <tr>
    <td class="tg-baqh">Replacement</td>
    <td class="tg-baqh" colspan="2"><br>generational with elitism<br>steady state<br></td>
  </tr>
  <tr>
    <td class="tg-baqh">Stop condition</td>
    <td class="tg-baqh" colspan="2"><br>number of generations<br>values of objective function<br>stall of objective function<br>custom or interactive (via `loopIO`)<br><br>time limit (via `loopIO`)<br><br>compound conditions (via `And` or `Or`)<br></td>
  </tr>
  <tr>
    <td class="tg-0lax">Logging</td>
    <td class="tg-0lax" colspan="2"><br>pure periodic (any monoid)<br>periodic with IO<br></td>
  </tr>
  <tr>
    <td class="tg-0lax">Constrained optimisation</td>
    <td class="tg-0lax" colspan="2"><br>constrained initialisation<br>constrained selection<br>death penalty</td>
  </tr>
  <tr>
    <td class="tg-0lax">Multiobjective optimisation</td>
    <td class="tg-0lax" colspan="2"><br>NSGA-II<br>constrained NSGA-II<br></td>
  </tr>
</table>
`^` non-adaptive: any function which doesn't depend on generation number

The list-like `type Genome a = [a]` makes it possible to encode a
range of data structures. Here are a couple of examples:

-   permutation encodings, where `a` is an integer, or some other `Enum`
    type
-   tree encodings, where `a` is a subtree type
-   hybrid encodings, where `a` is a sum type

Contributing
------------

There are many ways in which you can help to develop the library:

-   I'm not a native speaker of English. If you are, please proof-read
    and correct the comments and documentation.
-   Moo was designed with the potential of implementing custom genetic
    operators in mind. If you write new operators --- see `SelectionOp`,
    `CrossoverOp`, `MutationOp` for example --- or additional
    replacement strategies --- see `StepGA` --- please consider
    contributing them to the library. In the comments please give a
    reference to the academic work (with a DOI preferably) which
    introduced the method, explain when or why it should be used and
    provide some tests and examples if possible.
-   Implementing some methods (like adaptive genetic algorithms) will
    require changes to be made to some of the library types. Please
    discuss your approach first by lodging an issue.
-   Contribute examples. Solutions of known problems with known optima
    and interesting properties are preferable. Try to avoid examples
    which are too contrived.

An example
----------

Minimizing [Beale's
function](http://en.wikipedia.org/wiki/Test_functions_for_optimization)
which takes its minimum value at $f(3, 0.5) = 0$.

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

For more examples, see the [examples/](examples/) folder.
