module Exercises.Five.Six.Main where

import qualified Exercises.Five.Six.Matrix as Matrix

main :: IO ()
main = putStrLn.show $ Matrix.matrixProduct (Matrix.newMatrix 10 10) (Matrix.newMatrix 10 10)
