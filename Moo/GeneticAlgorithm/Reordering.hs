module Reordering
	( 
	-- Types
	module Moo.GeneticAlgorithm.Types
	-- Mutations
	,  swapMutate
	, listswapMutate
	, revMutate
	, blockSwapMutate
	, shuffelMutate
	, shiftMutate
	-- Crossovers
	, edgeCrossover
	-- initialization
	, initializeIntGenome
	-- apply
	, applyReordering
	-- others
	, module Moo.GeneticAlgorithm.Run
	) where
import Moo.GeneticAlgorithm.Types
import Moo.GeneticAlgorithm.Random
import Moo.GeneticAlgorithm.Run

import ReorderingMutations (swapMutate, listswapMutate, revMutate, blockSwapMutate, shuffelMutate, shiftMutate)
import ReorderingCrossOvers (edgeCrossover)

import Data.List (sortBy, permutations)
import Data.Ord (comparing)


applyReordering :: (Ord a) => [a] -> [b] -> [b]
applyReordering genome = map snd . sortBy (comparing fst) . zip genome

--TODO: Not a good initialization
initializeIntGenome :: Int -> Int -> Rand [Genome Int]
initializeIntGenome populationsize genomeLength = do
	return . take populationsize . map (take genomeLength) . permutations $ [0,1..] 

-- sometimes usefull for fittness functions
-- [1,2,3,4,5] -> [(1,2),(2,3),(3,4),(4,5)]
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs [_] = []
pairs (a:b:rest) = (a,b): pairs (b:rest)