module Exercises.FiveNine.AVLTree (
  AVLTree,
  emptyAVL,
  insert,
  delete
) where

data (Ord a, Show a) => AVLTree a =
  EmptyAVL | NodeAVL Int a (AVLTree a) (AVLTree a) deriving (Show)

isEmpty EmptyAVL = True
isEmpty _ = False

emptyAVL = EmptyAVL

insert :: (Ord a, Show a) => a -> AVLTree a -> AVLTree a
insert value emptyNode@EmptyAVL = NodeAVL 1 value emptyNode emptyNode
insert value (NodeAVL heightr v lf rt)
  | value < v =
    let
      newTree = NodeAVL ((max heightlf (height rt)) + 1) v newlf rt
      newlf@(NodeAVL heightlf valuelf _ _) = insert value lf
    in rebalance newTree (value < valuelf)
  | otherwise =
    let
      newTree = NodeAVL ((max heightrt (height lf)) + 1) v lf newrt
      newrt@(NodeAVL heightrt valuert _ _) = insert value rt
    in rebalance newTree (value > valuert)

delete :: (Ord a, Show a) => a -> AVLTree a -> AVLTree a
delete v' emptyNode@EmptyAVL = emptyNode
delete v' tree
  | not (contains tree v') = tree

delete v' tree@(NodeAVL _ v EmptyAVL rt)
  | v == v' = rt

delete v' (NodeAVL _ v lf EmptyAVL)
  | v == v' = lf

delete v' (NodeAVL _ v lf rt)
  | v' < v =
    let
      newTree = NodeAVL ((max (height newlf) (height rt)) + 1) v newlf rt
      newlf = delete v' lf
    in if (shouldRebalance newlf) then rebalance newTree (v' < (value newlf)) else newTree

  | v' > v =
    let
      newTree = NodeAVL ((max (height newrt) (height lf)) + 1) v lf newrt
      newrt = delete v' rt
    in if (shouldRebalance newrt) then rebalance newTree (v' > (value newrt)) else newTree

  | v' == v =
    let
      newTree = NodeAVL ((max (height lf) (height newrt)) + 1) minValueRt lf newrt
      newrt = delete minValueRt rt
      minValueRt = minValue rt
    in if (shouldRebalance newrt) then rebalance newTree (v' > (value newrt)) else newTree
  where
    shouldRebalance EmptyAVL = False
    shouldRebalance _ = True
    value (NodeAVL _ value _ _) = value

rebalance :: (Ord a, Show a) => AVLTree a -> Bool -> AVLTree a
rebalance EmptyAVL _ = EmptyAVL
rebalance tree@(NodeAVL _ _ lf rt) rotateOnce
  | ((height lf) - (height rt)) == 2 =
    if rotateOnce then rotateLeft tree else doubleRotateLeft tree
  | ((height rt) - (height lf)) == 2 =
    if rotateOnce then rotateRight tree else doubleRotateRight tree
  | otherwise = tree

contains :: (Ord a, Show a) => AVLTree a -> a -> Bool
contains EmptyAVL _ = False
contains (NodeAVL _ v lf rt) value
  | value == v = True
  | value < v = contains lf value
  | value > v = contains rt value

rotateRight :: (Ord a, Show a) => AVLTree a -> AVLTree a
rotateRight node@EmptyAVL = node
rotateRight node@(NodeAVL _ _ _ EmptyAVL) = node
rotateRight (NodeAVL heightr v lf (NodeAVL _ vrt rtlf rtrt)) =
  NodeAVL heightr vrt (NodeAVL ((max (height lf) (height rtlf)) + 1) v lf rtlf) rtrt

rotateLeft :: (Ord a, Show a) => AVLTree a -> AVLTree a
rotateLeft node@EmptyAVL = node
rotateLeft node@(NodeAVL _ _ EmptyAVL _) = node
rotateLeft (NodeAVL heightr v (NodeAVL _ vlf lflv lfrt) rt) =
  NodeAVL heightr vlf lflv (NodeAVL ((max (height lfrt) (height rt)) + 1) v lfrt rt)

doubleRotateLeft :: (Ord a, Show a) => AVLTree a -> AVLTree a
doubleRotateLeft node@(NodeAVL _ _ EmptyAVL _) = node
doubleRotateLeft node@(NodeAVL _ _ (NodeAVL _ _ _ EmptyAVL) _) = node
doubleRotateLeft (NodeAVL heightr v (NodeAVL _ vlf lflf (NodeAVL _ vlfrt lfrtlf lfrtrt)) rt) =
  NodeAVL heightr vlfrt
    (NodeAVL ((max (height lflf) (height lfrtlf)) + 1) vlf lflf lfrtlf)
    (NodeAVL ((max (height lfrtrt) (height rt)) + 1) v lfrtrt rt)

doubleRotateRight :: (Ord a, Show a) => AVLTree a -> AVLTree a
doubleRotateRight node@(NodeAVL _ _ _ EmptyAVL) = node
doubleRotateRight node@(NodeAVL _ _ _ (NodeAVL _ _ EmptyAVL _)) = node
doubleRotateRight (NodeAVL heightr v lf (NodeAVL _ vrt (NodeAVL _ vrtlf rtlflf rtlfrt) rtrt)) =
  NodeAVL heightr vrtlf
    (NodeAVL ((max (height lf) (height rtlflf)) + 1) v lf rtlflf)
    (NodeAVL ((max (height rtlfrt) (height rtrt)) + 1) vrt rtlfrt rtrt)

height :: (Ord a, Show a) => AVLTree a -> Int
height EmptyAVL = 0
height (NodeAVL height _ _ _) = height

minValue :: (Show a, Ord a) => AVLTree a -> a
minValue EmptyAVL = error "minValue of EmptyAVL is called"
minValue (NodeAVL _ value EmptyAVL _) = value
minValue (NodeAVL _ value lf _) = minValue lf
