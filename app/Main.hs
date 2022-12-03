module Main (main) where

import Lib
import System.IO  

main :: IO ()
main = do  
        runDay "res/day1ex1.txt" day1ex1
        runDay "res/day1ex1.txt" day1ex2
        
runDay :: String -> ([String] -> Int) -> IO ()
runDay filePath f = do  
        handle <- openFile filePath ReadMode
        contents <- hGetContents handle
        let l = lines contents
        print (f l)
        hClose handle   