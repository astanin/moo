-- This example uses SimpleEA library to solve linear equation
-- using genetic algorithm. A variation of the example using
-- floating point numbers as matrix coefficients and solution
-- elements.

import AI.SimpleEA
import AI.SimpleEA.Rand
import AI.SimpleEA.Utils
import Control.Monad
import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64
import Print (printHistoryAndBest)

n = 4             -- number of equations
range = (-10, 10) -- range of coefficients and solution entries
popsize = 500     -- population size
ndiscrete = 1000  -- discretization steps
maxiters = 10000  -- stop after maxiters iterations

-- create a random system of linear equations, return matrix and rhs
createSLE :: Int -> Rand ([[Double]], [Double], [Double])
createSLE n = do
  mat <- replicateM n $ replicateM n (getRandomR range) :: Rand [[Double]]
  xs <- replicateM n (getRandomR range)
  let rhs = mat `mult` xs
  return (mat, xs, rhs)

-- convert solution to bit encoding (genome)
toGenome :: [Double] -> [Bool]
toGenome = concatMap (encodeGrayReal range ndiscrete)

-- convert bit encoding (genome) to solution variables
fromGenome :: [Bool] -> [Double]
fromGenome = map (decodeGrayReal range ndiscrete) . splitEvery nbits
  where
  nbits = bitsNeeded (0,ndiscrete-1)

-- fitness function designed to minimize the norm of the residual
-- of the candidate solution.
fitness mat rhs bits _ =
    let xs = fromGenome bits
        residual = norm2 $ (mat `mult` xs) `minus` rhs
    in  negate residual

-- selection: tournament selection with elitism
select mat rhs pop =
    let keep = popsize `div` 10
        top = take keep (elite pop)
    in  do
      rest <- tournamentSelect 3 (popsize - keep) pop
      return (top ++ rest)

main = do
  (mat,solution,rhs, (pop, log)) <- runGA $ do
         -- random SLE problem
         (mat, solution, rhs) <- createSLE n
         -- initial population
         xs0 <- replicateM popsize $ replicateM n (getRandomR range)
         let p0 = evalFitness (fitness mat rhs) . map toGenome $ xs0
         -- digest function to keep log of evolution
         let digest p = (avgFitness p, maxFitness p)
         -- run for 10*200 generations, save digest every 10 iterations
         let stopCondition = FitnessStdev (<= 10.0) `Or` MaxFitness (>= (-0.1))
         r <- loopUntil' (stopCondition `Or` Iteration 200) digest p0 $
               iterateUntil (stopCondition `Or` Iteration 10) $
                  nextGeneration (fitness mat rhs)
                                 (select mat rhs)
                                 (twoPointCrossover 0.5)
                                 (pointMutate 0.35)
         return (mat, solution, rhs, r)
  printHistoryAndBest (show . fromGenome) pop log
  putStr $ unlines
    [ "# system matrix: " ++ show mat
    , "# system right hand side: " ++ show rhs
    , "# system solution: " ++ show solution ]

-- Matrix - vector product.
mult :: (Num a) => [[a]] -> [a] -> [a]
mult rows xs = map (sum . zipWith (*) xs) rows

-- Vector - vector sum and difference.
plus :: (Num a) => [a] -> [a] -> [a]
plus xs ys = zipWith (+) xs ys
minus :: (Num a) => [a] -> [a] -> [a]
minus xs ys = zipWith (-) xs ys

-- Vector norm.
norm2 :: (Num a, Real a) => [a] -> Double
norm2 = sqrt . fromRational . toRational . sum . map (^2)
