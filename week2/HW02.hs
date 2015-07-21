{-# OPTIONS_GHC -Wall #-}
module HW02 where

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches guess code = matches 0 guess code
    where matches :: Int -> Code -> Code -> Int
          matches acc (x:xs) (y:ys)
              | x == y = 1 + matches acc xs ys
              | otherwise = acc
          matches acc _ _ = acc


-- Exercise 2 -----------------------------------------

countColor :: Peg -> Code -> Int
countColor peg code = count 0 peg code
    where count :: Int -> Peg -> Code -> Int
          count acc peg (x:xs)
            | peg == x = 1 + count acc peg xs
            | otherwise = acc
          count acc _ _ = acc

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
    

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
matches = undefined

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove = undefined

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent = undefined

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes = undefined

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes = undefined

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
