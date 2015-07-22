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

-- Get the number of exact matches between the secret code and the guess
exactMatches :: Code -> Code -> Int
exactMatches guess secret = exactMatch 0 guess secret
    where exactMatch :: Int -> Code -> Code -> Int
          exactMatch acc (x:xs) (y:ys)
              | x == y = exactMatch (1 + acc) xs ys
              | otherwise = exactMatch acc xs ys
          exactMatch acc _ _ = acc 

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors (x:xs) = zipWith (+) (countColor x colors) (countColors xs)
    where countColor :: Peg -> [Peg] -> [Int]
          countColor peg col = zipWith colorMatch (replicate 6 peg) (col)
          colorMatch :: Peg -> Peg -> Int
          colorMatch peg1 peg2
            | peg1 == peg2 = 1
            | otherwise = 0
countColors _ = replicate 6 0

-- Count number of matches between the secret code and the guess
matches :: Code -> Code -> Int
matches guess secret = sum $ zipWith matches' (countColors guess) (countColors secret)
    where matches' :: Int -> Int -> Int
          matches' a b 
            | a < b = a
            | otherwise = b

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the secret code
getMove :: Code -> Code -> Move
getMove secret guess = Move guess e (m-e)
    where e = exactMatches secret guess
          m = matches guess secret

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move move exact nonexact) code =  (exact == checkExact) && (nonexact == checkNonExact)
    where Move _ checkExact checkNonExact = getMove code move

-- Exercise 5 -----------------------------------------

filterCodes :: Move -> [Code] -> [Code]
filterCodes move@(Move _ _ _) (code:codes) 
    | isConsistent move code = code : filterCodes move codes
    | otherwise = filterCodes move codes
filterCodes _ _ = []

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = 
allCodes n = concatMap allCodes' (allCodes (n - 1))
    where allCodes' :: Code -> [Code]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve = undefined

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
