module Exercises.Six.Eleven.Main (main) where

main :: IO ()
main = putStrLn.show $ quicksort [10, 9 .. 1]

-- inorder :: (Ord a, Show a) => AVLTree a -> [a]
-- inorder tree = inorder' tree []
--   where
--     inorder' EmptyAVL result = result
--     inorder' (NodeAVL _ value lf rt) result = inorder' lf (value:(inorder' rt result))

-- quicksort is derived from inorder using deforestation

quicksort :: (Ord a, Show a) => [a] -> [a]
quicksort list = quicksort' list []
  where
    quicksort' [] result = result
    quicksort' xss@(x:_) result = quicksort' smaller (x:(quicksort' larger result))
      where
        (smaller, larger) = split xss [] []
        split [] smaller larger = (smaller, larger)
        split (y:ys) smaller larger
          | y < x = split ys (y:smaller) larger
          | y > x = split ys smaller (y:larger)
          | otherwise = split ys smaller larger
