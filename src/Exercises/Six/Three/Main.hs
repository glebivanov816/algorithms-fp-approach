module Exercises.Six.Three.Main (main) where

main :: IO ()
main = putStrLn.show $ bubbleSort [5, 2, 4, 1, 3, 6, 0, 10, 123]

bubbleSort :: (Ord a) => [a] -> [a]
bubbleSort [] = []
bubbleSort [x] = [x]
bubbleSort list = bubbleSort' list (length list)
  where
    bubbleSort' list times
      | times == 0 = list
      | otherwise =
        let
          (exchangedItems, continue) = exchangeItems list times
        in
          if continue then bubbleSort' exchangedItems (times - 1) else exchangedItems

    exchangeItems list times = exchangeItems' list [] times False

    exchangeItems' [] result _ continue = (reverse result, continue)
    exchangeItems' [x] result _ continue = (reverse (x:result), continue)
    exchangeItems' (x:y:zs) result times continue
      | times == 0 = exchangeItems' zs (y:x:result) times continue
      | x == y = exchangeItems' (x:zs) (y:result) (times - 1) continue
      | x < y = exchangeItems' (y:zs) (x:result) (times - 1) True
      | x > y = exchangeItems' (x:zs) (y:result) (times - 1) True
