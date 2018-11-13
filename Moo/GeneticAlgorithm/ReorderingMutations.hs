module ReorderingMutations (swapMutate, listswapMutate, revMutate, blockSwapMutate, shuffelMutate, shiftMutate) where

import Moo.GeneticAlgorithm.Binary (CrossoverOp, Genome, MutationOp, getRandomR, Rand, shuffle)
import Moo.GeneticAlgorithm.Random (getDouble, getBool, withProbability, getNormal)
import Data.Ord (comparing)
import Data.List (sortBy, sort, group, groupBy, nub, permutations)
import Test.QuickCheck
import Control.Monad (liftM, replicateM)


swapMutate :: MutationOp a
swapMutate xs = 
	if length xs < 2 then error "Your list is too short to swap anything."
	else do
		[x1,x2,x3] <- randomSplitIn 3 xs
		if length x2 < 2 then swapMutate xs
		else
			return $ x1 ++ [last x2] ++ (init . tail) x2 ++ [head x2] ++ x3

listswapMutate :: MutationOp a
listswapMutate xs = do
	[x1,x2,x3,x4,x5] <- randomSplitIn 5 xs
	return $ x1 ++ x4 ++ x3 ++ x2 ++ x5

revMutate :: MutationOp a
revMutate xs = do
	[x1,x2,x3] <- randomSplitIn 3 xs
	return $ x1 ++ reverse x2 ++ x3

blockSwapMutate :: MutationOp a
blockSwapMutate xs = do
	[x1,x2] <- randomSplitIn 2 xs
	return $ x2 ++ x1

shuffelMutate :: MutationOp a
shuffelMutate xs = do 
	[x1,x2,x3] <- randomSplitIn 3 xs
	shuffledx2 <- shuffle x2
	return $ x1 ++ shuffledx2 ++ x3

shiftMutate :: MutationOp a
shiftMutate xs = do
	decide <- getBool
	let shift = if decide then rightShift else leftShift
	[x1,x2,x3] <- randomSplitIn 3 xs
	return $ x1 ++ shift x2 ++ x3

	where
		rightShift :: [a] -> [a]
		rightShift [] = []
		rightShift xs = [last xs] ++ init xs
		leftShift :: [a] -> [a]
		leftShift [] = []
		leftShift xs = tail xs ++ [head xs]

randomSplitIn :: Int -> [a] -> Rand [[a]]
randomSplitIn n xs = do
	indices <- randomIntListOfLengthAndSum id n (length xs)
	return $ splitHere indices xs

splitHere :: [Int] -> [a] -> [[a]]
splitHere [] [] = []
splitHere (0:ns) xs = []: splitHere ns xs 
splitHere _ [] 	= error "Your list was too short."
splitHere [] _	= error "Your list was too long."
splitHere (n:ns) xs
	| length xs < n	= error "Your indices are malformed"
	| otherwise 	= xs1 : splitHere ns xs2
	where
		(xs1, xs2) = splitAt n xs



randomIntListOfLengthAndSum :: (Double -> Double) -> Int -> Int -> Rand [Int]
randomIntListOfLengthAndSum distribution listlength listsum = 
	(return . scaleUp listsum . map distribution) =<< getListOfDoubles listlength
-- random List of Doubles between 0 and 1 (I hope getDouble is equally distributed)
getListOfDoubles :: Int -> Rand [Double]
getListOfDoubles 0 = return []
getListOfDoubles n = do
	d <- getDouble
	-- I am not shure about the borders of getDouble, so I make sure it is between 0 and 1
	let drem = remainer d
	ds <- getListOfDoubles (n-1)
	return (drem:ds)

-- assumed n >= 0
-- scaleUp n ds returns you a list ls of Ints, so that sum ls == n and the proportions are as konstant as possible
-- we use the method of the "greatest remainer" TODO: Source needed
scaleUp :: Int -> [Double] -> [Int]
scaleUp n ds = roundList n (scaleUpDoubles n ds)
	where		
		-- same thing as scaleUp, but with Doubles as result. This is much easier, since there is no rounding envolved
		scaleUpDoubles :: Int -> [Double] -> [Double]
		scaleUpDoubles n ds = map (\d -> (d* fromIntegral n)/ sum ds ) ds
		
		roundList :: Int -> [Double] -> [Int]
		roundList n ds = zipWith (+) fracseats fullseats
			where
				fullseats :: [Int]
				fullseats = map floor ds
		
				restseats :: Int
				restseats = n - sum fullseats
		
				fracseats :: [Int]
				fracseats = greatestRemainer restseats (map remainer ds)
		
		-- the greater your remaining percent the more
		greatestRemainer :: Int -> [Double] -> [Int]
		greatestRemainer n ds = (map fst . sortBy (comparing snd) . zip seatList . map snd . sortBy (comparing fst) . zip ds) [1..]
			where
				-- [0,0,0,0,0,0,0,0,0,0,0,0,0,0,1,1,1,1,1,1]
				seatList :: [Int]
				seatList = replicate (length ds - n) 0 ++ replicate n 1
				
-- the higher the more likly to get a seat
remainer :: Double -> Double
remainer d = d - (fromIntegral.floor) d




	
main = do
	print "It compiles."
	--quickCheckAll

quickCheckAll :: IO()
quickCheckAll = do
	quickCheck prop_IntRemainerIsZero
	quickCheck prop_OneParty
	quickCheck prop_NumberOfPartys
	quickCheck prop_ScaleUpRank

prop_IntRemainerIsZero :: Int -> Bool
prop_IntRemainerIsZero = (0==) . remainer . fromIntegral

prop_OneParty :: Int -> Double -> Property
prop_OneParty n d =
			d /= 0 ==>
			scaleUp n' [d] == [n']
			where
				n' = n `mod` (10^15)

prop_NumberOfPartys :: Int -> [Double] -> Bool
prop_NumberOfPartys n ds = (length . scaleUp n) ds == length ds

prop_ScaleUpRank :: Int -> [Double] -> Bool
prop_ScaleUpRank n ds = scaledUpDoubles == sort scaledUpDoubles
	where
		scaledUpDoubles = (scaleUp (abs n) . sort . map abs) ds