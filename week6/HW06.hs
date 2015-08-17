{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor()

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib num
    | num <= 0 = 0
    | num == 1 = 1
    | otherwise = fib (num - 1) + fib (num - 2)

fibs1 :: [Integer]
fibs1 = map fib [1..]

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs2 (tail fibs2)

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a stream) = a : streamToList stream

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a stream) = Cons (f a) (fmap f stream)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat a = Cons a (sRepeat a)

sIterate :: (a -> a) -> a -> Stream a
sIterate f a = Cons (f a) (sIterate f (f a))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons a stream) = Cons a . flip sInterleave stream

sTake :: Int -> Stream a -> [a]
sTake n (Cons a stream)
    | n <= 0 = []
    | otherwise = a : sTake (n-1) stream

-- Exercise 6 -----------------------------------------

nats :: Stream Integer
nats = Cons 0 $ fmap (+1) nats

ruler :: Stream Integer
ruler = ruler' 0
    where ruler' num = sInterleave (sRepeat num) $ ruler' (num + 1)

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand num  = sIterate randomNum num
    where randomNum n
            | n >= 1 = (1103515245 * n + 12345) `mod` 2147483648
            | otherwise = randomNum 1


-- Exercise 8 -----------------------------------------

{- Total Memory in use: 234 MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: 1 MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax [] = Nothing
minMax (x:xs) = Just (minMax' (x, x) xs)
    where minMax' (low, high) (y:ys) = low `seq` high `seq` minMax' (min y (low), max y (high)) ys
          minMax' pair _ = pair

{- Total Memory in use: 1 MB
    half copied during gc -}
minMax2 :: [Int] -> Maybe (Int, Int)
minMax2 [] = Nothing
minMax2 (x:xs) = Just (minMax' (x, x) xs)
    where minMax' (low, high) (y:ys) = lowest `seq` highest `seq` minMax' (lowest, highest) ys
            where lowest = min y low
                  highest = max y high
          minMax' pair _ = pair

{- Total Memory in use: 212 MB -}
minMaxFold :: [Int] -> Maybe (Int, Int)
minMaxFold [] = Nothing
minMaxFold (x:xs) = Just (foldl' (\(low, high) num -> (min low num, max high num)) base xs)
    where base = (x, x)

{- Total Memory in use: 274 MB -}
minMaxFoldr :: [Int] -> Maybe (Int, Int)
minMaxFoldr [] = Nothing
minMaxFoldr(x:xs) = Just (foldr (\num (low, high) -> (min low num, max high num)) base xs)
    where base = (x, x)

main :: IO ()
main = print $ minMax $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
