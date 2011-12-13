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
"AI.SimpleEA.Utils".

>select :: Int -> SelectionOp Bool
>select n = withScale sigmaScale $ rouletteSelect n

In our @main@ function we wrap the entire algorithm with 'runGA'
helper. It gives us access to the random number generator throughout
its @do@ block. We generate a random initial population of 100 genomes
with 'getRandomGenomes' function.

We use 'onePointCrossover' and 'pointMutate' functions to
provide simple 'CrossoverOp' and 'MutationOp' respectively. The we run
the algorithm for 41 generations with 'iterateHistoryM' function.  It
not only runs the algorithm, but also accumulates the history.

>main = do
>    let popsize = 11
>    let genomesize = 50
>    let elitesize = 1
>    (pop, history) <- runGA $ do
>       genomes <- getRandomGenomes popsize genomesize (False,True)
>       let pop0 = evalFitness countTrue genomes
>       let xover = onePointCrossover 0.33
>       let mutate = pointMutate 0.2
>       let step = nextGeneration elitesize countTrue
>                  (select (popsize-elitesize)) xover mutate
>       let digest p = (avgFitness p, maxFitness p)
>       loopUntil' (MaxFitness (>= fromIntegral genomesize)) digest pop0 step
>    printHistoryAndBest showBits pop history
