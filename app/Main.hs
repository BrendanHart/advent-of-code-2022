module Main (main) where

import Lib
import System.IO  
import Data.List.Split
import Data.List
import Data.Char
import Text.ParserCombinators.Parsec

main :: IO ()
main = do  
        runDay1 "res/day1ex1.txt" day1ex1
        runDay1 "res/day1ex1.txt" day1ex2
        runDay2 "res/day2ex1.txt" day2ex1
        runDay2 "res/day2ex1.txt" day2ex2
        runDay3 "res/day3ex1.txt" day3ex1
        runDay3 "res/day3ex1.txt" day3ex2
        runDay4 "res/day4ex1.txt" day4ex1
        runDay4 "res/day4ex1.txt" day4ex2
        runDay5 "res/day5ex.txt" day5ex1
        runDay5 "res/day5ex.txt" day5ex2        
        runDay6 "res/day6.txt" day6ex1        
        runDay6 "res/day6.txt" day6ex2        
        
runDay1 :: String -> ([String] -> Int) -> IO ()
runDay1 filePath f = do  
        handle <- openFile filePath ReadMode
        contents <- hGetContents handle
        let l = lines contents
        print (f l)
        hClose handle   

runDay2 :: String -> ([(Char, Char)] -> Int) -> IO ()
runDay2 filePath f = do  
        handle <- openFile filePath ReadMode
        contents <- hGetContents handle
        let l = lines contents
        print (f (map parseLine l))
        hClose handle   
    where
        parseLine :: String -> (Char, Char)
        parseLine (x : ' ' : y : []) = (x, y)

runDay3 :: String -> ([[Char]] -> Int) -> IO ()
runDay3 filePath f = do  
        handle <- openFile filePath ReadMode
        contents <- hGetContents handle
        let l = lines contents
        print (f l)
        hClose handle   

runDay4 :: String -> ([((Int, Int), (Int, Int))] -> Int) -> IO ()
runDay4 filePath f = do  
        handle <- openFile filePath ReadMode
        contents <- hGetContents handle
        let l = lines contents
        print (f (map transformed l))
        hClose handle   
    where
        transformed = readPairs . convert . map (convert . splitOn "-") . splitOn ","
        convert :: [a] -> (a, a)
        convert (a : b : _) = (a, b) 
        readPairs :: ((String, String), (String, String)) -> ((Int, Int), (Int, Int))
        readPairs ((a, b), (c, d)) = ((read a, read b), (read c, read d))

runDay5 :: String -> ([Stack Char] -> [BoxMove] -> String) -> IO ()
runDay5 filePath f = do  
        handle <- openFile filePath ReadMode
        contents <- hGetContents handle
        let l = lines contents
        print $ f (getStacks l) (map getMoves $ drop 10 l)
        hClose handle   
    where
        getStacks :: [String] -> [Stack Char]
        getStacks xs = filter (not . null) $ map (filter isAlpha) $ transpose (take 8 xs)
        readMoves :: Parser (Int, Int, Int)
        readMoves = do
                skipMany (noneOf "1234567890")
                x <- many1 digit
                skipMany (noneOf "1234567890")
                y <- many1 digit
                skipMany (noneOf "1234567890")
                z <- many1 digit
                return ((read x), (read y), (read z))
        getMoves :: String -> (Int, Int, Int)
        getMoves input = case parse readMoves "" input of
                Left err -> error "Failed to read line"
                Right val -> val

runDay6 :: String -> (String -> Int) -> IO ()
runDay6 filePath f = do  
        handle <- openFile filePath ReadMode
        contents <- hGetContents handle
        print $ f contents
        hClose handle   