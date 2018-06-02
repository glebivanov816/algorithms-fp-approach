module Exercises.Five.Six.Matrix (
  Matrix,
  newMatrix,
  matrixProduct
) where

import qualified Exercises.Five.Six.Table as Table

newtype Matrix = Mtx (Table.Table Int) deriving (Show)

type Vector = [Int]

newMatrix :: Int -> Int -> Matrix
newMatrix rows cols = Mtx $ Table.newTable [((i, j), i * j) | i <- [1 .. rows], j <- [1 .. cols]]

matrixProduct :: Matrix -> Matrix -> Matrix
matrixProduct (Mtx xs) (Mtx ys) = Mtx $ Table.newTable [((i, j), (valueOf i j)) | i <- rows, j <- cols]
  where
    rows = [(Table.minRow xs) .. (Table.maxRow xs)]
    cols = [(Table.minCol ys) .. (Table.maxCol ys)]
    valueOf row col = scalarProduct (Table.getRow xs row) (Table.getColumn ys col)

scalarProduct :: Vector -> Vector -> Int
scalarProduct xs ys = scalarProduct' xs ys 0
  where
    scalarProduct' [] ys result = result
    scalarProduct' xs [] result = result
    scalarProduct' (x:xs) (y:ys) result = scalarProduct' xs ys (result + x * y)
