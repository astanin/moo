{- An example of a genetic sorting algorithm
   contributed by Jøhannes Lippmann <code@schauderbasis.de> -}
import Moo.GeneticAlgorithm.Binary

import Control.Arrow (first)
import Control.Monad (when)
import Data.List (intercalate, sortBy)
import Data.Ord (comparing)

{-
Sorting a list (of Characters) using a genetic algorythm.
The output should be readable for human, but for a nice
plot with gnuplot you can paste this in "gensort.gnuplot"...

set terminal postscript eps enhanced color font 'Helvetica,10'
set xlabel 'Generations'
set ylabel 'sorting fittnes'
set output 'gensort.eps'
set key bottom box
plot 'output.txt' using 1:2 with lines lc rgb 'red' lw 4 title 'best value',\
     'output.txt' using 1:3 with lines lc rgb 'dark-grey' title 'median value',\
     'output.txt' using 1:4 with lines lc rgb 'grey' title 'worst value'

...and this in "gensort.sh"...

#!/bin/bash
ghc --make gensort.hs
time ./gensort > gensort.txt
gnuplot -p gensort.gnuplot
filelength=$(wc -l < gensort.txt)
echo "Final Generation: $(($filelength - 6))"
tail -5 gensort.txt

... and run it with ./gensort.sh
-}

-- list to be sorted
type Problem a = [a]
-- The Symetric Group of degree n: http://en.wikipedia.org/wiki/Symmetric_group
-- a memeber of S_n should change the order of the elements, not the elements itself
type S_n a = [a] -> [a]

-- sortingFittnes ls == 1 is aquivalent to ls == sort ls
sortingFittness :: (Ord a) => Problem a -> Genome Bool -> Double
sortingFittness problem bools =
      (fromIntegral . sortcount . makeFunktion problem bools) problem
      / (fromIntegral . twoOutOf . length) problem
  where
    sortcount :: (Ord a) => [a] -> Int
    sortcount (x:xs) = (sum . map (sortcount' x)) xs + sortcount xs
      where
        sortcount' :: (Ord a) => a -> a -> Int
        sortcount' x y
          | x > y   = 0
          | otherwise = 1
        --sortcount' x y = (length . filter id . (\x -> [x])) (x <= y)
    sortcount [] = 0

    twoOutOf :: Int -> Int
    twoOutOf 1 = 0
    twoOutOf n = n - 1 + twoOutOf (n-1)


problem = concat $ replicate 1 "fNortOfe"
genomesize = boolGenomeLengthForProblem problem
-- stopconditions (they are very high)
maxiters = 50000
timeLimit = 60 -- 1 minute

popsize :: Int
popsize = 10

selection :: SelectionOp a
selection = rouletteSelect 5

crossover :: CrossoverOp a
crossover = uniformCrossover 0.3

mutation :: MutationOp Bool
mutation = pointMutate 0.5

elitesize = 1

showGenome :: (Ord a, Show a) => Problem a -> Genome Bool -> String
showGenome problem bools = "Genome " ++ showBits bools
            ++ "\n(which eqals to " ++ (show . boolGenomeToIntGenome (length problem)) bools ++ ")"
            ++ "\nmakes " ++ show problem
            ++ "\nto " ++ (show . makeFunktion problem bools) problem
            ++ "\n(Sortingfittness: " ++ show (sortingFittness problem bools) ++ ")"
            where
              showBits :: [Bool] -> String
              showBits = concatMap (show . fromEnum)

geneticAlgorithm :: (Ord a, Show a) => Problem a -> IO (Population Bool)
geneticAlgorithm problem = do
  let fitness = sortingFittness problem
  let nextGen = nextGeneration Maximizing fitness selection elitesize crossover mutation
  runIO initializeBoolGenome $ loopIO
    [DoEvery 1 (logStats problem), TimeLimit timeLimit]
    (Or (Generations maxiters) (IfObjective (any (==1))))
    nextGen

-- Gnuplotreadable statistics for 1 Generation
logStats :: (Ord a, Show a) => Problem a -> Int -> Population Bool -> IO ()
logStats problem iterno pop = do
  when (iterno == 0) $
    putStrLn "# generation medianValue bestValue"
  let gs = map takeGenome . bestFirst Maximizing $ pop  -- genomes
  let best = head gs
  let median = gs !! (length gs `div` 2)
  let worst = last gs
  putStrLn $ unwords  [ show iterno
            , (take 6 . show . sortingFittness problem) best
            , (take 6 . show . sortingFittness problem) median
            , (take 6 . show . sortingFittness problem) worst
            , show ((makeFunktion problem best) problem)
            ]

main :: IO()
main = do
  finalPop <- geneticAlgorithm problem
  let winner = takeGenome . head . bestFirst Maximizing $ finalPop
  putStrLn $ showGenome problem winner
  return ()




-- Dealing with Genomes

-- Number of Bools that are needed to represent a member of the intGenome for a problem of a given length
initializeBoolGenome :: Rand [Genome Bool]
initializeBoolGenome = getRandomBinaryGenomes popsize genomesize

-- in which range are the numbers that are sorted to generate the S_n
-- the bigger d the more permutations are evaluated, so they are more equaly distributed
intRange :: Int -> (Int, Int)
intRange n = (0,d*n)
  where
    d = 10

boolsPerInt :: Int -> Int
boolsPerInt = bitsNeeded . intRange

-- Genome Bool -> Genome Int -> ([a]->[a])

-- If I want to solve this Problem, how long will my Genome of Bools need to be?
boolGenomeLengthForProblem :: Problem a -> Int
boolGenomeLengthForProblem problem = problemLength * boolsPerInt problemLength
  where
    problemLength = length problem

makeFunktion :: Problem a -> [Bool] -> S_n a
makeFunktion problem boolGenome
  | length boolGenome /= boolGenomeLengthForProblem problem = error $ "Didn't get the correct number of bools for makeing a Funktion. \n Bools Needed: " ++ show (boolGenomeLengthForProblem problem) ++ "\n Bools gotten: " ++ show (length boolGenome)
  | otherwise = (intGenomeToS_n . boolGenomeToIntGenome problemLength) boolGenome
  where
    problemLength = length problem

boolGenomeToIntGenome :: Int -> [Bool] -> [Int]
boolGenomeToIntGenome problemLength boolGenome
  | length boolGenome `mod`  boolsPerInt problemLength /= 0 = error "Problem Converting [Bool] -> [Int] "
  | otherwise = map (decodeGray (intRange problemLength)) (splitEvery (boolsPerInt problemLength) boolGenome)

intGenomeToS_n :: (Ord a) => [a] -> S_n b
intGenomeToS_n as = map snd . sortBy (comparing fst) . zip as
