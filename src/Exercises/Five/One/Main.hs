module Exercises.Five.One.Main where

import qualified Exercises.Five.One.PriorityQueue as PQ
import qualified Exercises.Five.One.Point as Point

main :: IO ()
main = putStrLn.show $ foldr (\point queue -> PQ.enqueue point queue) PQ.emptyQueue Point.generatePoints
