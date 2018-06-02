module Exercises.Six.Seven.Main (main) where

main :: IO ()
main = putStrLn.show $ regularMergeSort worstCaseList

worstCaseList :: [Int]
worstCaseList = [1, 3 .. 10] ++ [2, 4 .. 10]

regularMergeSort :: (Ord a) => [a] -> [a]
regularMergeSort [] = []
regularMergeSort [x] = [x]
regularMergeSort list = merge (regularMergeSort firstPart) (regularMergeSort secondPart)
  where
    firstPart = take center list
    secondPart = drop center list
    center = div (length list) 2

    merge [] ys = ys
    merge xs [] = xs
    merge l1@(x:xs) l2@(y:ys)
      | x <= y = x:(merge xs l2)
      | otherwise = y:(merge ys l1)

bottomUpMergeSort :: (Ord a) => [a] -> [a]
bottomUpMergeSort [] = []
bottomUpMergeSort [x] = [x]
bottomUpMergeSort list = mergeSort splittedList
  where
    splittedList = split list []
    split [] result = result
    split (x:xs) result = split xs ([x]:result)

    mergeSort [result] = result
    mergeSort list = mergeSort (mergePairs list)

    mergePairs [] = []
    mergePairs [x] = [x]
    mergePairs (a:b:rest) = (merge a b):rest

    merge [] ys = ys
    merge xs [] = xs
    merge l1@(x:xs) l2@(y:ys)
      | x <= y = x:(merge xs l2)
      | otherwise = y:(merge ys l1)
