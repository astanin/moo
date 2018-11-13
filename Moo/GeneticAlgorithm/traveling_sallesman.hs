import Reordering
import Data.List (sortBy)
import Data.Ord (comparing)

-- This one is neccescary for all the functions that should be in Reordering but are not (yet)
-- such as Selections 
import Moo.GeneticAlgorithm.Binary


-- how much does it cost to travel from a to b
roadCost :: (Int, Int) -> Double
roadCost (a,b) = (costList !! a) !! b

-- a matrix of dimension citys X citys
costList :: [[Double]]
costList =  [ [0.39987,0.99363,0.48044,0.74801,7.01106,0.53062,0.76280,0.31503,0.56092,2.36610]
			, [0.20854,0.64304,0.97168,0.14401,5.90881,0.73724,0.38740,0.81714,0.44270,0.69404]
			, [0.36724,0.34072,0.82010,3.78421,0.42394,0.44683,0.79694,0.66550,0.75300,0.66500]
			, [0.15891,0.24411,0.35355,0.25769,2.19011,0.12094,0.49855,0.83530,0.44275,0.27326]
			, [0.40955,3.57548,0.81909,0.87116,0.98246,0.90539,0.27536,0.26896,0.79010,0.97057]
			, [0.90195,0.22583,0.91092,0.47696,0.72954,0.73374,0.41783,0.89910,0.89136,0.45286]
			, [9.25324,8.08620,0.16877,3.15063,7.14468,0.79607,0.91033,0.89157,0.81961,0.34512]
			, [0.41108,0.47445,0.55982,0.47753,0.79543,0.34781,0.92973,0.97550,6.37965,0.65307]
			, [0.19750,0.55189,0.74052,0.55952,0.37908,0.57073,0.57096,0.63539,0.31313,0.50520]
			, [0.11348,0.61098,2.84733,0.97944,0.49017,8.82357,0.62606,0.94343,0.81197,0.85248]
			]

type Tour = [Int]
citys = 10

tourcost :: Tour -> Double
tourcost tour =
	sum . map roadCost . roads $ tour ++ [head tour]
	where
		roads :: Tour -> [(Int, Int)]
		roads [] = []
		roads [_] = []
		roads (a:b:rest) = (a,b): roads (b:rest)

popsize = 8
elitesize = 1

selection = rouletteSelect (popsize - elitesize)
crossover = edgeCrossover 2
mutation = shiftMutate
step = nextGeneration Minimizing tourcost selection elitesize crossover mutation
stop =  Generations 100
initialize = initializeIntGenome popsize 10


main = do
  population <- runGA initialize (loop stop step)
  print (head . sortBy (comparing snd) $ population)