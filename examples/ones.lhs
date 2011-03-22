The aim of this GA is to maximize the number of @True@ values in a
list (bitstring). The fitness of the bitstring is defined to be the
number of @True@ values it contains.

>import AI.SimpleEA
>import AI.SimpleEA.Utils
>import AI.SimpleEA.Rand
>
>import Data.List
>import System.Environment (getArgs)
>import Control.Monad (unless, liftM, replicateM)

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
>              in  replicateM n (rouletteSelect scaled)

In our @main@ function we wrap the entire algorithm with 'runGA'
helper. It gives us access to the random number generator throughout
its @do@ block. We generate a random initial population of 100 genomes
with 'getRandomGenomes' function.

We use 'onePointCrossover' and 'pointMutate' functions to
provide simple 'CrossoverOp' and 'MutationOp' respectively. The we run
the algorithm for 41 generations with 'iterateHistoryM' function.  It
not only runs the algorithm, but also accumulates the history.

>main = do
>    args <- getArgs
>    gs <- runGA $ do
>       genomes <- getRandomGenomes 100 20 (False,True)
>       let pop = evalFitness countTrue genomes
>       let xover = onePointCrossover 0.33
>       let mutate = pointMutate 0.01
>       let step = nextGeneration countTrue select xover mutate
>       reverse `liftM` iterateHistoryM 41 step pop

Average and maximum fitness values and fitness standard deviation are
then calculated for each generation and written to a file if a file
name was provided as a command line argument. This data can then be
plotted with, e.g. gnuplot (<http://www.gnuplot.info/>).

>    let gen_avg_best_std = getPlottingData gs
>    if (null args)
>      then putStr gen_avg_best_std
>      else writeFile (head args) gen_avg_best_std
