module Exercises.Six.Ten.Main (main) where

import Exercises.Six.Ten.AVLTree as AVLTree

main :: IO ()
main = putStrLn.show $ AVLTree.fromList [7, 6 .. 1]

treeSort :: (Show a, Ord a) => [a] -> [a]
treeSort [] = []
treeSort [x] = [x]
treeSort list = (AVLTree.inorder).(AVLTree.fromList) $ list
