module Exercises.Five.Two.Main where

import qualified Exercises.Five.Two.PriorityQueue as PQ

main :: IO ()
main = putStrLn.show $ foldr (\num queue -> PQ.enqueue num queue) (PQ.emptyQueue (>)) [1 .. 10]
