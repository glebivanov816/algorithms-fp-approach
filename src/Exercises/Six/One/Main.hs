module Exercises.Six.One.Main (
  main
) where

main :: IO ()
main = putStrLn.show $ insertionSort $ [52, 50 .. 31] ++ [84, 83 .. 65]

insertionSort :: (Ord a) => [a] -> [a]
insertionSort list = foldr insert [] list
  where
    insert y [] = [y]
    insert y list@(z:zs)
      | y <= z = y:list
      | otherwise = z:(insert y zs)
