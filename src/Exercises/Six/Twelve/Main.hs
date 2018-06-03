module Exercises.Six.Twelve.Main (main) where

import Data.Array

main :: IO ()
main = putStrLn.show $ radixSort 3 (0, 9) generateInts

type Value a =  [a]
type Bucket a = [Value a]
type Buckets a = Array a (Bucket a)

generateInts :: [Value Int]
generateInts = [[a, b, c] | a <- [1 .. 2], b <- [1 .. 2], c <- [1 .. 2]]

radixSort :: (Ord a, Ix a) => Int -> (a, a) -> [Value a] -> [Value a]
radixSort 0 _ values = values
radixSort radixPlusOne sortRange values = radixSort radix sortRange sortedByRadix
  where
    radix = radixPlusOne - 1
    sortedByRadix
      | radix == 0 = concatenatedBuckets
      | otherwise = reverse concatenatedBuckets
      where
        concatenatedBuckets = concatBuckets (makeBuckets sortRange radix values)

concatBuckets :: (Ord a) => Buckets a -> [Value a]
concatBuckets = concat.elems

makeBuckets :: (Ord a, Ix a) => (a, a) -> Int -> [Value a] -> Buckets a
makeBuckets sortRange radix values = accumArray (flip (:)) [] sortRange assocs
  where
    assocs = map makeAssoc values
    makeAssoc value = (value !! radix, value)
