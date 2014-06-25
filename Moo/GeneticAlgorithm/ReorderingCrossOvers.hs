module ReorderingCrossOvers (edgeCrossover) where

import Moo.GeneticAlgorithm.Binary (CrossoverOp, Genome, MutationOp, Rand, shuffle)
import Data.List (sort, group, groupBy, nub)
import Control.Monad (liftM, replicateM)

-- for testing
{-
import Control.Monad.Mersenne.Random
import System.Random.Mersenne.Pure64
printRand :: (Show a) => Rand a -> IO a
printRand randX = do
	myPureMT <- newPureMT
	return (evalRandom randX myPureMT)
main = printRand . liftM fst . edgeCrossover 2 $ [[1,2,3,4,5], [1,4,3,2,5]]
-}

{-
edge crossover, found in Karsten Weickers 'Evolutionaere Algorythmen' (second edition) as 'Kantenrekombination' (Teubner, german book)
tldr: take n parents and find the most common starting knot (choose one of them randomly if necessary).
Then look to the parents: Which are the most common succedors to your knot? Choose one of them as your next knot and redo this procedere.
Every knot should be taken exactly once.
This computation takes really long to compute.
Make sure the parents are just permutations of each other. Otherwise you are in trouble. 
Examples:
* parents = [[1,2,3,4,5]]
Since there is only one parent, the child will be an exact copy of his mother/father.
* parents = [[1,2,3,4,5], [1,4,3,2,5]]
The starting point is clearly 1. After thar there is a tie, since 2 follows 1 as often (once) as 4 does. We choose 4 randomly, so we have [1,4,?,?,?]
After 4 there is a tie again (5 or 3). We choose 5: [1,4,5,?,?]
5 does not have an succedor in his parents, so we can take any unused number. 3 or 4? We take 3. [1,4,5,3,?]
There is only one remaining knot. After this one we have [1,4,5,3,2]. Finished
Other possible childs would be: [1,2,3,4,5], [1,2,5,3,4], [1,2,5,4,3], [1,4,3,2,5], [1,4,5,3,2]
-}
edgeCrossover :: (Eq a, Ord a) => Int -> CrossoverOp a
edgeCrossover n potentialParents = do
	if n > length potentialParents
		then return ([], potentialParents)
	else do
		let (parents, unusedParents) = splitAt n potentialParents
		-- not sure how many children moo expects from one bunch of parents. I guess it would be n.
		let numberOfChildren = n
		children <- replicateM numberOfChildren (haveSexForEdgeCrossover parents)
		return (children, unusedParents)
	where

		haveSexForEdgeCrossover :: (Eq a, Ord a) => [[a]] -> Rand [a]
		haveSexForEdgeCrossover parents = do
			let parentalEdges = concatMap edges parents
			startpoint <- chooseRandom . filterByNumber . map head $ parents
			let possibleKnots = nub . concat $ parents
			-- this is dangerous if the parents aren't made of the exactly same knots
			childTail <- findPathFrom startpoint possibleKnots parentalEdges
			return (startpoint : childTail)
		
		-- for haveSexForEdgeCrossover
		-- can deal with (startKnot `elem` knotsToPass) and parentalEdges from or to bad knots
		findPathFrom :: (Eq a, Ord a) => a -> [a] -> [(a,a)] -> Rand [a]
		findPathFrom startKnot knotsToPass parentalEdges = do
			let remainingEdges = filterEdgesByKnots knotsToPass parentalEdges
			let remainingKnots = filter (/= startKnot) knotsToPass
			if null remainingKnots
				then return []
			else do  
				nextEdge <- chooseEdge startKnot remainingKnots remainingEdges
				let nextKnot = snd nextEdge
				restPath <- findPathFrom nextKnot remainingKnots remainingEdges
				return (nextKnot:restPath)

			where
				-- tries to find the best edge of the given list of edges
				-- if there is no edge that fits, one is made up 
				chooseEdge :: (Eq a, Ord a) => a -> [a] -> [(a,a)] -> Rand (a,a)
				chooseEdge _ [] _ = error "No knots to choose from."
				chooseEdge startKnot [onlyRemainingKnot] _ = return $ (startKnot, onlyRemainingKnot)
				chooseEdge startKnot remainingKnots remainingParentalEdges = do
					let favouritEdges = filterByStartingpoint startKnot remainingParentalEdges
					if (not . null) favouritEdges then
						chooseRandom . filterByDecicions startKnot . filterByNumber $ favouritEdges
					else do
						randomKnot <- chooseRandom remainingKnots
						return (startKnot, randomKnot)

maxArg :: (Ord b) => (a -> b) -> [a] -> [a]
maxArg f xs = [x | x <- xs, f x == maximum (map f xs)]

minArg :: (Ord b) => (a -> b) -> [a] -> [a]
minArg f xs = [x | x <- xs, f x == minimum (map f xs)]

chooseRandom :: [b] -> Rand b
chooseRandom = liftM head . shuffle 

-- only return those that appear most often in the list.
filterByNumber :: (Ord a, Eq a) => [a] -> [a]
filterByNumber = concat . maxArg length . group . sort

-- only returns Edges that live inside a pool of given knots
filterEdgesByKnots :: (Eq a) => [a] -> [(a,a)] -> [(a,a)]
filterEdgesByKnots goodKnots candidateEdges = [edge | edge <- candidateEdges, fst edge `elem` goodKnots, snd edge `elem` goodKnots ] 

filterByStartingpoint :: (Eq a) => a -> [(a,a)] -> [(a,a)]
filterByStartingpoint x = filter (hasStartpoint x)
	where
		hasStartpoint :: (Eq a) => a -> (a,a) -> Bool
		hasStartpoint x (y,_) = x == y

-- take those edges where the knot you come out has a small number of outgoing edges
filterByDecicions :: (Eq a) => a -> [(a,a)] -> [(a,a)]
filterByDecicions x xs = map (fromTo x) . minArg (decicions xs) . poissibleEndPoints . filterByStartingpoint x $ xs
	where
		-- if I start here, in how many unique directions can I go?
		decicions :: (Eq a) => [(a,a)] -> a -> Int
		decicions xs x = length . poissibleEndPoints . filterByStartingpoint x $ xs

		-- how many unique endpoints are there
		-- (not startpointspecific)
		poissibleEndPoints :: (Eq a) =>[(a,a)] -> [a]
		poissibleEndPoints = nub . map snd

		-- fromTo x y creates an edge that leads from x to y. Its basicly a type constructor
		fromTo :: a -> a -> (a,a)
		fromTo x y = (x,y)





-- edges of one parent
edges :: [a] -> [(a,a)]
edges [] = []
edges [_] = []
edges (x:y:zs) = (x,y): edges (y:zs)





