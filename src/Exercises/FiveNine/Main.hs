module Exercises.FiveNine.Main (main) where

import qualified Exercises.FiveNine.AVLTree as AVLTree

main :: IO ()
main = putStrLn.show $ AVLTree.delete 10 $ foldr AVLTree.insert AVLTree.emptyAVL [1 .. 32]
