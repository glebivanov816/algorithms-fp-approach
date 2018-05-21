module Exercises.FiveSix.Table(
  Table,
  newTable,
  findTable,
  updTable
) where

import Data.Array

newtype Table b v = Tbl (Array (b, b) [((b, b), v)]) deriving (Show)

newTable :: [((b, b), v)] -> Table b v
newTable pairs = Tbl $ array (lo, hi) pairs
  where
    indices = map fst pairs
    lo = minimum indices
    hi = maximum indices

findTable :: (Table b v) -> (b, b) -> v
findTable (Tbl array) key
  | elem key array = array ! key
  | otherwise = error "Value doesn't exist in table"

updTable :: ((b, b), v) -> Table b v -> Table b v
updTable pair (Tbl array) = Tbl $ array // [pair]
