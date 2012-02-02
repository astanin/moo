{- |

Ersatz linear algebra.

-}

module Moo.GeneticAlgorithm.LinAlg
  ( minus
  , plus
  , scale
  , dot
  , norm2
  , proj
  , normalize
  ) where

minus :: Num a => [a] -> [a] -> [a]
minus xs ys  = zipWith (-) xs ys
plus :: Num a => [a] -> [a] -> [a]
plus xs ys   = zipWith (+) xs ys
scale :: Num a => a -> [a] -> [a]
scale a xs   = map (a*) xs
dot :: Num a => [a] -> [a] -> a
dot xs ys    = sum $ zipWith (*) xs ys
norm2 :: (Num a, Floating a) => [a] -> a
norm2 xs     = sqrt $ dot xs xs
proj :: (Num a, Fractional a) => [a] -> [a] -> [a]
proj xs dir  = ( dot xs dir / dot dir dir ) `scale` dir
normalize :: (Num a, Floating a, Fractional a) => [a] -> [a]
normalize xs = let a = norm2 xs in (1.0/a) `scale` xs

