module Day1 (day1ex1, day1ex2) where

import Data.List

day1ex1 :: [String] -> Int
day1ex1 lines = findHighestCalories lines 0

findHighestCalories :: [String] -> Int -> Int
findHighestCalories = findHighestCaloriesAcc 0

findHighestCaloriesAcc :: Int -> [String] -> Int -> Int
findHighestCaloriesAcc _ [] currentHighest = currentHighest
findHighestCaloriesAcc currentCount ("" : xs) currentHighest 
    | currentCount > currentHighest = findHighestCaloriesAcc 0 xs currentCount
    | otherwise = findHighestCaloriesAcc 0 xs currentHighest
findHighestCaloriesAcc currentCount (x : xs) currentHighest = findHighestCaloriesAcc newCount xs currentHighest
    where
        newCount = currentCount + (read x)


day1ex2 :: [String] -> Int
day1ex2 lines = findSumOfTopThreeElves lines

countCaloriesPerElf :: [String] -> [Int]
countCaloriesPerElf lines = countCaloriesPerElfHelper [] 0 lines 

countCaloriesPerElfHelper :: [Int] -> Int -> [String] -> [Int]
countCaloriesPerElfHelper counts currentCount [] = counts
countCaloriesPerElfHelper counts currentCount ("" : xs) = countCaloriesPerElfHelper (currentCount : counts) 0 xs
countCaloriesPerElfHelper counts currentCount (x : xs) = countCaloriesPerElfHelper counts (currentCount + (read x)) xs

findSumOfTopThreeElves :: [String] -> Int
findSumOfTopThreeElves = sum . take 3 . sortBy (flip compare) . countCaloriesPerElf