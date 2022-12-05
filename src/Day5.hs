module Day5 (day5ex1, day5ex2, BoxMove, Stack) where

import Data.List

type Stack a = [a]
type BoxMove = (Int, Int, Int)

day5ex1 :: [Stack Char] -> [BoxMove] -> String
day5ex1 stacks moves = (transpose (foldl (applyMove reverse) stacks moves)) !! 0

applyMove :: (Stack Char -> Stack Char) -> [Stack Char] -> BoxMove -> [Stack Char]
applyMove orderFunction stacks (amount, from, to) = replaceStacks stacks [] (from-1) (to-1) 0 newFromStack newToStack
    where
        fromStack = stacks!!(from-1)
        toStack = stacks!!(to-1)
        (newFromStack, newToStack) = moveBetweenStacks orderFunction amount fromStack toStack

moveBetweenStacks :: (Stack Char -> Stack Char) -> Int -> Stack Char -> Stack Char -> (Stack Char, Stack Char)
moveBetweenStacks orderFunction amount fromStock toStack = (drop amount fromStock, (orderFunction $ take amount fromStock) ++ toStack)

replaceStacks :: [Stack Char] -> [Stack Char] -> Int -> Int -> Int -> Stack Char -> Stack Char -> [Stack Char]
replaceStacks [] acc _ _ _ _ _ = reverse acc
replaceStacks (x : xs) acc fromIndex toIndex currentIndex newStackFrom newStackTo
    | currentIndex == fromIndex = replaceStacks xs (newStackFrom : acc) fromIndex toIndex (currentIndex + 1) newStackFrom newStackTo
    | currentIndex == toIndex = replaceStacks xs (newStackTo : acc) fromIndex toIndex (currentIndex + 1) newStackFrom newStackTo
    | otherwise = replaceStacks xs (x : acc) fromIndex toIndex (currentIndex + 1) newStackFrom newStackTo

day5ex2 :: [Stack Char] -> [BoxMove] -> String
day5ex2 stacks moves = (transpose (foldl (applyMove id) stacks moves)) !! 0