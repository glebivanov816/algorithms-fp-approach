module Exercises.Five.Two.PriorityQueue (
  PriorityQueue,
  emptyQueue,
  queueEmpty,
  queueFront,
  enqueue,
  dequeue
) where

type CompareFunction a = a -> a -> Bool

data PriorityQueue a = PQ [a] (CompareFunction a)

instance (Show a) => Show (PriorityQueue a) where
  show (PQ items _) = show items

emptyQueue :: (Ord a) => CompareFunction a -> PriorityQueue a
emptyQueue compareFn = PQ [] compareFn

queueEmpty :: (Ord a) => PriorityQueue a -> Bool
queueEmpty (PQ [] _) = True
queueEmpty _ = False

queueFront :: (Ord a) => PriorityQueue a -> a
queueFront (PQ [] _) = error "queueFront on empty PQ is called"
queueFront (PQ (x:_) _) = x

enqueue :: (Ord a) => a -> PriorityQueue a -> PriorityQueue a
enqueue y (PQ items compareFn) = PQ (insert items) compareFn
  where
    insert [] = [y]
    insert (x:xs) = if compareFn x y then y:x:xs else x:(insert xs)

dequeue :: (Ord a) => PriorityQueue a -> PriorityQueue a
dequeue (PQ (x:xs) compareFn) = PQ xs compareFn
