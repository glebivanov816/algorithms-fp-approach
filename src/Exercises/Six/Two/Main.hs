module Exercises.Six.Two.Main (
  main
) where

import Data.Char

main :: IO ()
main = putStrLn.show $ insertionSort generatePeople

insertionSort :: (Ord a) => [a] -> [a]
insertionSort list = foldr insert [] list
  where
    insert y [] = [y]
    insert y list@(z:zs)
      | y <= z = y:list
      | otherwise = z:(insert y zs)

generatePeople :: [(String, Int)]
generatePeople = [([chr (i - 1), chr i, chr (i + 1)], i) | i <- [90 .. 120]]
