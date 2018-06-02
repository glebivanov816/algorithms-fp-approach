module Exercises.Six.Nine.Main (main) where

import qualified Exercises.Six.Nine.Heap as Heap

main :: IO ()
main = do
  let list = [12, 63, 21, 15, 64, 96, 66, 52, 20, 33, 90, 19]
  putStrLn.show $ Heap.fromList list
  putStrLn.show $ heapSort list

heapSort :: (Ord a) => [a] -> [a]
heapSort [] = []
heapSort [x] = [x]
heapSort list = reverse (flushHeap (Heap.fromList list) [])
  where
    flushHeap heap result
      | Heap.heapEmpty heap = result
      | otherwise = flushHeap (Heap.deleteHeap heap) ((Heap.findHeap heap):result)
