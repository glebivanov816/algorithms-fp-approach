module Exercises.FiveOne.Main where

import qualified Exercises.FiveOne.PriorityQueue as PQ
import qualified Exercises.FiveOne.Point as Point

main :: IO ()
main = putStrLn.show $ foldr (\point queue -> PQ.enqueue point queue) PQ.emptyQueue Point.generatePoints
