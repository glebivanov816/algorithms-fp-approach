module Exercises.Six.Six.Main (main) where

main :: IO ()
main = putStrLn.show $ quickSort [5, 2, 4, 1, 3, 6, 0, 10, 123]

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort list = quickSort' list []
  where
    quickSort' [] result = result
    quickSort' list result =
      let
        median = roughMedian list
        (lower, upper) = split median list
      in quickSort' lower (median : (quickSort' upper result))

    roughMedian [x] = x
    roughMedian list =
      let
        candidates = [head list, last list, middle]
        middle = list !! (div (length list) 2)
        roughMedian' [x, y, z]
          | (x >= y) && (x <= z) || (x <= y) && (x >= z) = x
          | (z >= y) && (z <= x) || (z <= y) && (z >= x) = z
          | otherwise = y
      in roughMedian' candidates

    split x xs = split' x xs [] []
    split' x [] lower upper = (lower, upper)
    split' x (y:ys) lower upper
      | y < x = split' x ys (y:lower) upper
      | y > x = split' x ys lower (y:upper)
      | otherwise = split' x ys lower upper
