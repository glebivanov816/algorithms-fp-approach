module Exercises.FiveSeven.Main (main) where

import qualified Exercises.FiveSeven.Heap as Heap

main :: IO ()
main = putStrLn.show $ Heap.fromList [1 .. 20]
