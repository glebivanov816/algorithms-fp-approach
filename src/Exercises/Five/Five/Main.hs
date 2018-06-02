module Exercises.Five.Five.Main (main) where

import qualified Exercises.Five.Five.Set as MultiSet

main :: IO ()
main = putStrLn.show $ MultiSet.union (buildSet 1 10) (buildSet 1 5)

buildSet :: Int -> Int -> MultiSet.Set Int
buildSet start end = MultiSet.fromList (foldr (++) [] [[x, x] | x <- [start .. end]])
