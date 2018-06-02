module Exercises.Five.Four.Main (main) where

import qualified Exercises.Five.Four.Set as Set

main :: IO ()
main = putStrLn.show $ Set.union (buildSet 56 60) (buildSet 1 5)

buildSet :: Int -> Int -> Set.Set Int
buildSet start end = Set.fromList [start .. end]
