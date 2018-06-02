module Exercises.Five.One.PriorityQueue (
  PriorityQueue,
  emptyQueue,
  queueEmpty,
  queueFront,
  enqueue,
  dequeue
) where

import qualified Exercises.Five.One.Heap as Heap

newtype PriorityQueue a = PQ (Heap.Heap a) deriving (Show)

emptyQueue :: (Ord a) => PriorityQueue a
emptyQueue = PQ Heap.emptyHeap

queueEmpty :: (Ord a) => PriorityQueue a -> Bool
queueEmpty (PQ heap) = Heap.heapEmpty heap

queueFront :: (Ord a) => PriorityQueue a -> a
queueFront (PQ heap)
  | Heap.heapEmpty heap = error "queueFront on empty PQ is called"
  | otherwise = Heap.findHeap heap

enqueue :: (Ord a) => a -> PriorityQueue a -> PriorityQueue a
enqueue a (PQ heap) = PQ (Heap.insertHeap a heap)

dequeue :: (Ord a) => PriorityQueue a -> PriorityQueue a
dequeue (PQ heap) = PQ (Heap.deleteHeap heap)
