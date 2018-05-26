module Exercises.FiveEight.Main (main) where

import qualified Exercises.FiveEight.Heap as Heap

main :: IO ()
main = putStrLn.show $ Heap.fromList [1 .. 20]
