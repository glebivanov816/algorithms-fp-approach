module Exercises.Six.Twelve.Main (main) where

main :: IO ()
main = putStrLn.show $ radixSort generateNumbers

generateNumbers :: [[Int]]
generateNumbers = [[a, b, c] | a <- [1 .. 2], b <- [1 .. 2], c <- [1 .. 2]]

radixSort :: (Ord a) => [[a]] -> [[a]]
radixSort list = list
