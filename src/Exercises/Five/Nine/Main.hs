module Exercises.Five.Nine.Main (main) where

import qualified Exercises.Five.Nine.AVLTree as AVLTree

main :: IO ()
main = putStrLn.show $ AVLTree.delete 10 $ foldr AVLTree.insert AVLTree.emptyAVL [1 .. 32]
