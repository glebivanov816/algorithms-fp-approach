module Exercises.Five.Seven.Heap (
  Heap,
  emptyHeap,
  heapEmpty,
  findHeap,
  deleteHeap,
  insertHeap,
  mergeHeap,
  fromList
) where

data Heap a = EmptyHp | Hp a Int (Heap a) (Heap a) deriving (Show)

fromList :: (Ord a) => [a] -> Heap a
fromList (x:[]) = Hp x 1 EmptyHp EmptyHp
fromList xs = mergeHeap (fromList (take center xs)) (fromList (drop center xs))
  where
    center = div (length xs) 2

emptyHeap :: (Ord a) => Heap a
emptyHeap = EmptyHp

heapEmpty :: (Ord a) => Heap a -> Bool
heapEmpty EmptyHp = True
heapEmpty _ = False

findHeap :: (Ord a) => Heap a -> a
findHeap EmptyHp = error "Find on EmptyHp is called"
findHeap (Hp value rank _ _) = value

deleteHeap :: (Ord a) => Heap a -> Heap a
deleteHeap EmptyHp = error "Delete on EmptyHp is called"
deleteHeap (Hp value rank left right) = mergeHeap left right

insertHeap :: (Ord a) => a -> Heap a -> Heap a
insertHeap newValue heap = mergeHeap (Hp newValue 1 EmptyHp EmptyHp) heap

mergeHeap :: (Ord a) => Heap a -> Heap a -> Heap a
mergeHeap heap EmptyHp = heap
mergeHeap EmptyHp heap = heap
mergeHeap heap1@(Hp x _ l1 r1) heap2@(Hp y _ l2 r2)
  | x <= y = makeHeap x l1 (mergeHeap r1 heap2)
  | otherwise = makeHeap y l2 (mergeHeap r2 heap1)

makeHeap :: (Ord a) => a -> Heap a -> Heap a -> Heap a
makeHeap value heap@(Hp _ rank _ _) EmptyHp = Hp value (rank + 1) heap EmptyHp
makeHeap value EmptyHp heap@(Hp _ rank _ _) = Hp value (rank + 1) heap EmptyHp
makeHeap value heap1@(Hp _ rank1 _ _) heap2@(Hp _ rank2 _ _)
  | rank1 >= rank2 = Hp value (rank2 + 1) heap1 heap2
  | otherwise = Hp value (rank1 + 1) heap2 heap1
