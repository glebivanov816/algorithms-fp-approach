module Exercises.Five.Eight.Main (main) where

import qualified Exercises.Five.Eight.Heap as Heap

main :: IO ()
main = putStrLn.show $ Heap.fromList [1 .. 20]
