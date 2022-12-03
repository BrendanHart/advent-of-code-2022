module Day2 (day2ex1, day2ex2) where

import Data.List

data Choice = Rock | Paper | Scissors deriving (Eq)
data Result = Win | Lose | Draw deriving (Eq)

readChoices :: (Char , Char) -> (Choice, Choice)
readChoices (x, y) = ((parseOpponentChoice x), (parseMyChoice y))

readChoiceAndResult :: (Char , Char) -> (Choice, Result)
readChoiceAndResult (x, y) = ((parseOpponentChoice x), (parseResult y))

parseOpponentChoice :: Char -> Choice
parseOpponentChoice 'A' = Rock
parseOpponentChoice 'B' = Paper
parseOpponentChoice 'C' = Scissors

parseMyChoice :: Char -> Choice
parseMyChoice 'X' = Rock
parseMyChoice 'Y' = Paper
parseMyChoice 'Z' = Scissors

parseResult :: Char -> Result
parseResult 'X' = Lose
parseResult 'Y' = Draw
parseResult 'Z' = Win

day2ex1 :: [(Char, Char)] -> Int
day2ex1 xs = calculateScore (map readChoices xs)

calculateScore :: [(Choice, Choice)] -> Int
calculateScore xs = sum (map fullScore xs)
    where
        roundOutcomeScore :: (Choice, Choice) -> Int
        roundOutcomeScore (Rock, Paper) = 6
        roundOutcomeScore (Scissors, Rock) = 6
        roundOutcomeScore (Paper, Scissors) = 6
        roundOutcomeScore (x, y)
            | x == y = 3
            | otherwise = 0

        choiceScore :: (Choice, Choice) -> Int
        choiceScore (_, Rock) = 1
        choiceScore (_, Paper) = 2
        choiceScore (_, Scissors) = 3

        fullScore :: (Choice, Choice) -> Int
        fullScore x = (roundOutcomeScore x) + (choiceScore x)

replaceWithChoice :: (Choice, Result) -> (Choice, Choice)
replaceWithChoice (Rock, Win) = (Rock, Paper)
replaceWithChoice (Paper, Win) = (Paper, Scissors)
replaceWithChoice (Scissors, Win) = (Scissors, Rock)
replaceWithChoice (Rock, Lose) = (Rock, Scissors)
replaceWithChoice (Paper, Lose) = (Paper, Rock)
replaceWithChoice (Scissors, Lose) = (Scissors, Paper)
replaceWithChoice (x, Draw) = (x, x)

day2ex2 :: [(Char, Char)] -> Int
day2ex2 xs = calculateScore (map replaceWithChoice (map readChoiceAndResult xs))
