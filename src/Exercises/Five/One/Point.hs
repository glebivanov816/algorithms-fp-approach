module Exercises.Five.One.Point where

data Point = Pt Int Int deriving (Show, Eq)

instance Ord Point where
  b <= a
    | (distance a) > (distance b) = False
    | otherwise = True

generatePoints :: [Point]
generatePoints = [Pt a b | a <- [1 .. 5], b <- [1 .. 5]]

distance :: Point -> Float
distance (Pt a b) = sqrt $ fromIntegral (a * a + b * b)
