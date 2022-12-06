module Day6 (day6ex1, day6ex2) where

import Data.List

day6ex1 :: String -> Int
day6ex1 = indexAfterNSuccessiveUniqueCharacters 4 0 "" 0

indexAfterNSuccessiveUniqueCharacters :: Int -> Int -> String -> Int -> String -> Int
indexAfterNSuccessiveUniqueCharacters targetUniqueCharsLength index _ uniqueCharsLength [] = if uniqueCharsLength == targetUniqueCharsLength then index else error "Could not find unique chars"
indexAfterNSuccessiveUniqueCharacters targetUniqueCharsLength index uniqueChars uniqueCharsLength (x : xs) 
    | uniqueCharsLength == targetUniqueCharsLength = index
    | otherwise = indexAfterNSuccessiveUniqueCharacters targetUniqueCharsLength (index + 1) newUniqueChars (length newUniqueChars) xs
        where
            seenIndex = elemIndex x uniqueChars  
            newUniqueChars = case seenIndex of
                Nothing -> x : uniqueChars
                (Just y) -> x : take y uniqueChars


day6ex2 :: String -> Int
day6ex2 = indexAfterNSuccessiveUniqueCharacters 14 0 "" 0