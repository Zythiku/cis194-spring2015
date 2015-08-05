{-# OPTIONS_GHC -Wall #-} module HW04 where

import Data.List

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P [1, 0]

-- Exercise 2 ----------------------------------------

skip0 :: (Num a, Eq a) => [a] -> [a]
skip0 (p) = dropWhile (==0) p

instance (Num a, Eq a) => Eq (Poly a) where
    P (a:as) == P (y:ys) = length (skip0 as) == length (skip0 ys) && a == y && P (as) == P (ys)
    P [] == P [] = True
    _ == _ = False
 
-- Exercise 3 -----------------------------------------

coefficent :: (Num a, Eq a, Show a)  => a -> String
coefficent n 
    | n == 1 = ""
    | otherwise = show n

showExp :: (Num a, Eq a, Show a) => a -> String
showExp n
    | n == 0 = ""
    | n == 1 = "x"
    | otherwise = "x^" ++ show n

showPolynomial :: (Num a, Eq a, Show a) => [a] -> [String]
showPolynomial (a:as) = (coefficent a ++ showExp (length (as))) : (showPolynomial $ skip0 as)
showPolynomial _ = [] 

instance (Num a, Eq a, Show a) => Show (Poly a) where
    show (P a)
        | length poly == 0 = "0"
        | otherwise = intercalate " + " poly
        where poly = showPolynomial $ skip0 a 
    


-- Exercise 4 -----------------------------------------

zipWith' :: Num a => (a -> a -> a) -> [a] -> [a] -> [a]
zipWith' op a b = zipWith op (a') (b')
    where a' = replicate (max (length b - length a) 0) 0 ++ a
          b' = replicate (max (length a - length b) 0) 0 ++ b


plus :: Num a => Poly a -> Poly a -> Poly a
plus (P a) (P b) = P (zipWith' (+) a b)


-- Exercise 5 -----------------------------------------

times' :: (Num a) => Int -> a -> [a] -> [a]
times' index num (a:as)
    | length as == 0 = a * num : replicate index 0
    | otherwise = a * num : times' index num as
times' _ _ [] = []

timesAll :: (Num a) => [a] -> [a] -> [Poly a]
timesAll (a:as) (bs) = P (times' (length as) a bs) : timesAll as bs
timesAll [] _ = []

times :: Num a => Poly a -> Poly a -> Poly a
times (P a) (P b) = sum (timesAll a b)

-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P a) = P (map negate a)
    fromInteger a = P [fromInteger a]
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------

applyP :: Num a => Poly a -> a -> a
applyP (P (a:as)) n = a * n^(length as) + (applyP (P as) n)
applyP _ _ = 0

-- Exercise 8 -----------------------------------------

class Num a => Differentiable a where
    deriv  :: a -> a
    nderiv :: Int -> a -> a
    nderiv n f 
        | n > 1 = nderiv (n-1) (deriv f)
        | otherwise = deriv f

-- Exercise 9 -----------------------------------------
instance (Num a, Enum a) => Differentiable (Poly a) where
    deriv (P []) = P []
    deriv (P a) = (P (deriv' (init a)))
        where deriv' :: (Num a) => [a] -> [a]
              deriv' [] = []
              deriv' (b:bs) = b * (fromIntegral (length bs) + 1) : deriv' bs

