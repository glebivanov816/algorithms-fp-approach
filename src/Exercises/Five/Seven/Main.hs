module Exercises.Five.Seven.Main (main) where

import qualified Exercises.Five.Seven.Heap as Heap

main :: IO ()
main = putStrLn.show $ Heap.fromList [1 .. 20]
