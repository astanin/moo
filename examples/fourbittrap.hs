{-

Ackley's 4-bit trap function f(u) is

    f(u) = 4, if u = 4,
    f(u) = 3-u, if u < 4.

where u is the number of ones in a 4-bit substring.

Concatenation of multiple trap functions is known to be a
difficult problem for binary genetic algorithms and requires
big populations to find the solution (all ones).

This example maximizes 4 concatenated 4-bit trap functions.

-}


import Moo.GeneticAlgorithm.Binary


import Control.Arrow (first)


popsize = 320
genomesize = 4*4
maxiters = 10000


trapFunction :: [Bool] -> Double
trapFunction bits =
    let u = length . filter (id) $ bits
        n = length bits
    in  if u == n
        then fromIntegral $ n
        else fromIntegral $ n - 1 - u


fitness :: [Bool] -> Double
fitness = sum . map trapFunction . split4
  where
    split4 [] = []
    split4 xs = let (a,b) = splitAt 4 xs
                in  a : split4 b


initialize = getRandomBinaryGenomes popsize genomesize
select = stochasticUniversalSampling popsize
crossover = onePointCrossover 0.5
mutate = pointMutate 0.01
evolve = loop ((Generations maxiters) `Or` converged) $
         nextGeneration Maximizing fitness select 0 crossover mutate
  where
    converged = IfObjective $ \fitvals -> maximum fitvals == minimum fitvals


main = do
    pop <- runGA initialize evolve
    mapM_ (print . first showBits) (take 1 $ bestFirst Maximizing pop)
  where
    showBits = map (\b -> if b then '1' else '_')