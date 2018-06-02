module Exercises.Six.Five.Main (main) where

main :: IO ()
main = putStrLn.show $ thresholdedQuicksort 9 $ [5, 2, 4, 1, 3, 6, 0, 10, 123]

thresholdedQuicksort :: (Ord a) => Int -> [a] -> [a]
thresholdedQuicksort threshold list
  | (length list) < threshold = insertionSort list
  | otherwise = quickSort list

insertionSort :: (Ord a) => [a] -> [a]
insertionSort list = foldr insert [] list
  where
    insert x [] = [x]
    insert x list@(y:ys)
      | x <= y = x:list
      | otherwise = y:(insert x ys)

quickSort :: (Ord a) => [a] -> [a]
quickSort [] = []
quickSort [x] = [x]
quickSort list = quickSort' list []
  where
    quickSort' [] result = result
    quickSort' (x:xs) result =
      let
        (lower, upper) = split x xs
      in quickSort' lower (x : (quickSort' upper result))

    split x xs = split' x xs [] []
    split' x [] lower upper = (lower, upper)
    split' x (y:ys) lower upper
      | y <= x = split' x ys (y:lower) upper
      | otherwise = split' x ys lower (y:upper)
