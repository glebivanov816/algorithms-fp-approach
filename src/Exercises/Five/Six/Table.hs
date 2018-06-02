module Exercises.Five.Six.Table(
  Table,
  newTable,
  findTable,
  updTable,
  maxCol,
  maxRow,
  minCol,
  minRow,
  getRow,
  getColumn
) where

import Data.Array

newtype Table v = Tbl (Array (Int, Int) v) deriving (Show)

newTable :: [((Int, Int), v)] -> Table v
newTable assocs = Tbl $ array (lo, hi) assocs
  where
    indices = map fst assocs
    lo = minimum indices
    hi = maximum indices

findTable :: Table v -> (Int, Int) -> v
findTable (Tbl array) key = array ! key

updTable :: ((Int, Int), v) -> Table v -> Table v
updTable assoc (Tbl array) = Tbl $ array // [assoc]

getRow :: Table v -> Int -> [v]
getRow table@(Tbl array) i = elems $ ixmap (minCol table, maxCol table) (\j -> (i, j)) array

getColumn :: Table v -> Int -> [v]
getColumn table@(Tbl array) j = elems $ ixmap (minRow table, maxRow table) (\i -> (i, j)) array

maxCol :: Table v -> Int
maxCol (Tbl array) = snd.snd $ bounds array

minCol :: Table v -> Int
minCol (Tbl array) = snd.fst $ bounds array

maxRow :: Table v -> Int
maxRow (Tbl array) = fst.snd $ bounds array

minRow :: Table v -> Int
minRow (Tbl array) = fst.fst $ bounds array
