module Exercises.FiveSix.Main where

import qualified Exercises.FiveSix.Matrix as Matrix

main :: IO ()
main = putStrLn.show $ Matrix.matrixProduct (Matrix.newMatrix 10 10) (Matrix.newMatrix 10 10)
