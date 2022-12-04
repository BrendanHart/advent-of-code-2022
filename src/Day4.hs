module Day4 (day4ex1, day4ex2) where

day4ex1 :: [((Int, Int), (Int, Int))] -> Int
day4ex1 ranges = length (filter eitherFullyContained ranges)

eitherFullyContained :: ((Int, Int), (Int, Int)) -> Bool
eitherFullyContained (range1, range2) = (range1 `fullyContains` range2) || (range2 `fullyContains` range1)

fullyContains :: (Int, Int) -> (Int, Int) -> Bool
fullyContains (a, b) (x, y) = a <= x && b >= y

day4ex2 :: [((Int, Int), (Int, Int))] -> Int
day4ex2 ranges = length (filter overlaps ranges)

overlaps :: ((Int, Int), (Int, Int)) -> Bool
overlaps ((a, b), (x, y)) = b >= x && a <= y