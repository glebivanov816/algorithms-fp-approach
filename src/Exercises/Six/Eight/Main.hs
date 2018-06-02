module Exercises.Six.Eight.Main (main) where

main :: IO ()
main = putStrLn.show $ tripleMergeSort [10, 9 .. 1]

tripleMergeSort :: (Ord a) => [a] -> [a]
tripleMergeSort [] = []
tripleMergeSort [x] = [x]
tripleMergeSort list@[x, y] = if x > y then [y, x] else list
tripleMergeSort list = tripleMerge (tripleMergeSort firstPart) (tripleMergeSort secondPart) (tripleMergeSort thirdPart)
  where
    firstPart = take third list
    secondPart = take third (drop third list)
    thirdPart = drop (third * 2) list
    third = div (length list) 3

    tripleMerge [] ys zs = merge ys zs
    tripleMerge xs [] zs = merge xs zs
    tripleMerge xs ys [] = merge xs ys
    tripleMerge xss@(x:xs) yss@(y:ys) zss@(z:zs)
      | x == localMinimum = x:(tripleMerge xs yss zss)
      | y == localMinimum = y:(tripleMerge ys xss zss)
      | z == localMinimum = z:(tripleMerge zs xss yss)
        where
          localMinimum = minimum [x, y, z]

    merge [] ys = ys
    merge xs [] = xs
    merge xss@(x:xs) yss@(y:ys)
      | x <= y = x:(merge xs yss)
      | otherwise = y:(merge ys xss)
