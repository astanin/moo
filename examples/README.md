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
    (basic NSGA-II)

  * [mop_kursawe.hs](mop_kursawe.hs) Kursawe function (n=3), a multiobjective problem
    with a discontinuous and non-convex Pareto set
    (constrained NSGA-II)

  * [mop_constr2.hs](mop_constr2.hs) a constrained multiobjective problem from (Deb, 2002),
    a part of the unconstrained Pareto-optimal region is not feasible
    (constrained NSGA-II with niching)

Examples of binary GA:

  * [knapsack.hs](knapsack.hs) 0-1 knapsack problem.
    (A basic GA with logging in IO and time limit)
