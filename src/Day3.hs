module Day3 (day3ex1, day3ex2) where

import Data.Char

splitIntoCompartments' :: [Char] -> Int -> Int -> ([Char], [Char]) -> ([Char], [Char])
splitIntoCompartments' [] x y acc = acc
splitIntoCompartments' (x : xs) y z (a, b)
    | y < z = splitIntoCompartments' xs (y + 1) z (x : a, b)
    | otherwise = splitIntoCompartments' xs (y + 1) z (a, x : b)

splitIntoCompartments :: [Char] -> ([Char], [Char])
splitIntoCompartments xs = splitIntoCompartments' xs 0 (div (length xs) 2) ([], [])

findCharacterInBothCompartments :: ([Char], [Char]) -> Char
findCharacterInBothCompartments (x : xs, ys) 
    | x `elem` ys = x
    | otherwise = findCharacterInBothCompartments (xs, ys)

mapToScore :: Char -> Int
mapToScore a = if isLower a then ord a - 96 else ord a - 38

day3ex1 :: [[Char]] -> Int
day3ex1 = sum . map (mapToScore . findCharacterInBothCompartments . splitIntoCompartments)

group :: Int -> [a] -> [[a]]
group _ [] = []
group n l
  | n > 0 = (take n l) : (group n (drop n l))
  | otherwise = error "Negative or zero n"

mapToTuple :: [String] -> (String, String, String)
mapToTuple (x : y : z : _) = (x, y, z)

findCommonCharacter :: (String, String, String) -> Char
findCommonCharacter ((x : xs), ys, zs)
    | x `elem` ys && x `elem` zs = x
    | otherwise = findCommonCharacter (xs, ys, zs)

day3ex2 :: [String] -> Int
day3ex2 = sum . map (mapToScore . findCommonCharacter . mapToTuple) . group 3