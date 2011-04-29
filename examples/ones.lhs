The aim of this GA is to maximize the number of @True@ values in a
list (bitstring). The fitness of the bitstring is defined to be the
number of @True@ values it contains.

>import AI.SimpleEA
>import AI.SimpleEA.Utils
>import AI.SimpleEA.Rand
>import Print (printHistoryAndBest)
>import Codec.Binary.Gray.List (showBits)

The @countTrue@ function is our 'FitnessFunction' and simply returns
the number of @True@ values in the list.

>countTrue :: FitnessFunction Bool
>countTrue g _ = (fromIntegral . length . filter id) g

The @select@ function is our 'SelectionOp'. It uses sigma-scaled,
fitness-proportionate selection. 'sigmaScale' is defined in
"AI.SimpleEA.Utils". By first taking the four best genomes (by using
the 'elite' function) we get elitism and ensure that the maximum
fitness never decreases (unless affected by a mutation).

>select :: SelectionOp Bool
>select gs = select' (take 4 $ elite gs)
>    where scaled = zip (map fst gs) (sigmaScale (map snd gs))
>          select' gs' =
>              let n = 2 * (length gs `div` 2 + 1) -- n >= length gs, n is even
>              in  rouletteSelect n scaled

In our @main@ function we wrap the entire algorithm with 'runGA'
helper. It gives us access to the random number generator throughout
its @do@ block. We generate a random initial population of 100 genomes
with 'getRandomGenomes' function.

We use 'onePointCrossover' and 'pointMutate' functions to
provide simple 'CrossoverOp' and 'MutationOp' respectively. The we run
the algorithm for 41 generations with 'iterateHistoryM' function.  It
not only runs the algorithm, but also accumulates the history.

>main = do
>    let popsize = 10
>    let genomesize = 20
>    (pop, history) <- runGA $ do
>       genomes <- getRandomGenomes popsize genomesize (False,True)
>       let pop0 = evalFitness countTrue genomes
>       let xover = onePointCrossover 0.33
>       let mutate = pointMutate 0.1
>       let step = nextGeneration countTrue select xover mutate
>       let digest p = (avgFitness p, maxFitness p)
>       loopUntil' (Iteration 50) digest pop0 step
>    printHistoryAndBest showBits pop history
