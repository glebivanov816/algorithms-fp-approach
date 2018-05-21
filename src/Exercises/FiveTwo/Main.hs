module Exercises.FiveTwo.Main where

import qualified Exercises.FiveTwo.PriorityQueue as PQ

main :: IO ()
main = putStrLn.show $ foldr (\num queue -> PQ.enqueue num queue) (PQ.emptyQueue (>)) [1 .. 10]
