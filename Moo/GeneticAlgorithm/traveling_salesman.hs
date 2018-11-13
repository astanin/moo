import Reordering
import Data.List (sortBy)
import Data.Ord (comparing)

-- This one is neccescary for all the functions that should be in Reordering but are not (yet)
-- such as Selections


import Moo.GeneticAlgorithm.Binary



type Tour = [Int]
citys :: Int
citys = length costList

-- how much does it cost to travel from a to b
roadCost :: (Int, Int) -> Double
roadCost (a,b) = (costList !! a) !! b


returnFromTour :: Tour -> Tour
returnFromTour tour = tour ++ [head tour]

tourcost :: Tour -> Double
tourcost tour =
	sum . map roadCost . pairs . returnFromTour $ tour

-- best way: [8,1,5,7,6,9,2,0,3,4] and return to 3 (costs only 9.60587)
-- a matrix of dimension n x n
costList :: [[Double]]
costList =  [ [1.39987,1.99363,1.48044,0.94801,1.01106,1.53062,1.76280,1.31503,1.56092,1.36610]
			, [1.20854,1.64304,1.97168,1.14401,1.90881,0.93724,1.38740,1.81714,1.44270,1.69404]
			, [0.96724,1.34072,1.82010,1.78421,1.42394,1.44683,1.79694,1.66550,1.75300,1.66500]
			, [1.15891,1.24411,1.35355,1.25769,0.99011,1.12094,1.49855,1.83530,1.44275,1.27326]
			, [1.40955,1.57548,1.81909,1.87116,1.98246,1.90539,1.27536,1.26896,0.99010,1.97057]
			, [1.90195,1.22583,1.91092,1.47696,1.72954,1.73374,1.41783,0.99910,1.89136,1.45286]
			, [1.25324,1.08620,1.16877,1.15063,1.14468,1.79607,1.91033,1.89157,1.81961,0.94512]
			, [1.41108,1.47445,1.55982,1.47753,1.79543,1.34781,0.92973,1.97550,1.37965,1.65307]
			, [1.19750,0.95189,1.74052,1.55952,1.37908,1.57073,1.57096,1.63539,1.31313,1.50520]
			, [1.11348,1.61098,0.94733,1.97944,1.49017,1.82357,1.62606,1.94343,1.81197,1.85248]
			]

popsize = 10
elitesize = 1

selection = rouletteSelect (popsize - elitesize)
crossover = edgeCrossover 2
mutation = shiftMutate
step = nextGeneration Minimizing tourcost selection elitesize crossover mutation
stop =  IfObjective (\values -> (minimum values) < 10)
initialize = initializeIntGenome popsize 10


main = do
  population <- runGA initialize (loop stop step)
  print (head . sortBy (comparing snd) $ population)