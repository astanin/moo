Examples
========

Examples of real-coded GAs:

  * [beale.hs](beale.hs) Beale function
    (basic GA)

  * [rosenbrock.hs](rosenbrock.hs) Rosenbrock function
    (basic GA with pure logging)

  * [schaffer2.hs](schaffer2.hs) Schaffer function #2
    (steady-state GA with niching)

  * [cp_sphere2.hs](cp_sphere2.hs) constrained 2D sphere function over a convex set
    (GA with a death penalty)

  * [cp_himmelblau.hs](cp_himmelblau.hs) constrained Himmelblau function over a non-convex set
    (GA with niching and constrained tournament selection)

  * [mop_minsum_maxprod.hs](mop_minsum_maxprod.hs) a simple multiobjective problem
    (basic NSGA-II, logging hypervolume evolution in IO)

  * [mop_kursawe.hs](mop_kursawe.hs) Kursawe function, a multiobjective problem
    with a discontinuous and non-convex Pareto set
    (constrained NSGA-II)

  * [mop_constr2.hs](mop_constr2.hs) a constrained multiobjective problem from (Deb, 2002),
    a part of the unconstrained Pareto-optimal region is not feasible
    (constrained NSGA-II with niching)

Examples of binary GAs:

  * [knapsack.hs](knapsack.hs) 0-1 knapsack problem
    (A basic GA with logging in IO and time limit)

  * [fourbittrap.hs](fourbittrap.hs) concatenation of N-bit trap
    functions is a difficult problem for genetic algorithms, and
    requires to use large populations
    (A basic GA with convergence check)

Examples of integer-coded GAs:

  * [ilp.hs](ilp.hs) an integer programming problem (a constrained GA
    with genomes as lists of integers and a custom mutation operator)


How to build examples within Cabal sandbox
------------------------------------------

For sandboxed builds, if you initialized the sandbox in the top-level
directory of the moo source distribution as

    cabal sandbox init

and compiled the library with

    cabal install

then

  * In the `examples/` directory run

        cabal sandbox init --sandbox=../.cabal-sandbox

  * Build examples like

        cabal exec ghc -- --make example_file.hs

    instead of

        ghc --make example_file.hs
