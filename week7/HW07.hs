{-# LANGUAGE MonadComprehensions, RecordWildCards #-}
{-# OPTIONS_GHC -Wall #-}
module HW07 where

import Prelude hiding (mapM)
import Cards

import Control.Monad hiding (mapM, liftM)
import Control.Monad.Random
import Data.Functor
import Data.Monoid
import Data.Vector (Vector, cons, (!), (!?), (//))
import System.Random

import qualified Data.Vector as V


-- Exercise 1 -----------------------------------------

liftM :: Monad m => (a -> b) -> m a -> m b
liftM f ma = do
    a <- ma
    return $ f a

swapV :: Int -> Int -> Vector a -> Maybe (Vector a)
swapV i1 i2 v = liftM2 swapV' (v !? i1) (v !? i2)
    where swapV' v1 v2 = v // [(i1, v2), (i2, v1)]


-- Exercise 2 -----------------------------------------

mapM :: Monad m => (a -> m b) -> [a] -> m [b]
mapM f = sequence . map f

getElts :: [Int] -> Vector a -> Maybe [a]
getElts is v = mapM (v !?) is

-- Exercise 3 -----------------------------------------

type Rnd a = Rand StdGen a

randomElt :: Vector a -> Rnd (Maybe a)
randomElt v
    | V.length v == 0 = return Nothing
    | otherwise = (v !?) <$> getRandomR (0, V.length v - 1)

-- Exercise 4 -----------------------------------------

randomVec :: Random a => Int -> Rnd (Vector a)
randomVec n = V.replicateM n getRandom

randomVecR :: Random a => Int -> (a, a) -> Rnd (Vector a)
randomVecR n r = V.replicateM n (getRandomR r)

-- Exercise 5 -----------------------------------------

shuffle' :: Int -> Vector a -> Rnd (Vector a)
shuffle' i v
    | i < 0 = return v
    | otherwise = (swap <$> randomRange) >>= shuffle' (i - 1)
    where randomRange = getRandomR (0, i)
          swap j = v // [(i, v ! j), (j, v ! i)]

shuffle :: Vector a -> Rnd (Vector a)
shuffle v
    | V.length v == 0 = return v
    | otherwise = shuffle' (V.length v - 1) v

-- Exercise 6 -----------------------------------------

partitionAt :: Ord a => Vector a -> Int -> (Vector a, a, Vector a)
partitionAt v p = (h, n, t)
    where n = v ! p
          h = V.ifilter (\i x -> i /= p && x < n) v
          t = V.ifilter (\i x -> i /= p && x >= n) v

-- Exercise 7 -----------------------------------------

-- Quicksort
quicksort :: Ord a => [a] -> [a]
quicksort [] = []
quicksort (x:xs) = quicksort [ y | y <- xs, y < x ]
                   <> (x : quicksort [ y | y <- xs, y >= x ])

qsort :: Ord a => Vector a -> Vector a
qsort v
    | V.length v == 0 = V.empty
    | otherwise = qsort h <> (V.cons n $ qsort t)
    where (h, n, t) = partitionAt v 0

-- Exercise 8 -----------------------------------------

--Much slower than qSort'--
qsortR :: Ord a => Vector a -> Rnd (Vector a)
qsortR v = (\(h, n, t) -> qsort h <> V.cons n (qsort t)) <$> (partitionAt v) <$> randomI
    where randomI = getRandomR (0, V.length v - 1)

qsortR' :: Ord a => Vector a -> Rnd (Vector a)
qsortR' v
    | V.length v == 0 = return V.empty
    | otherwise = do
            i <- getRandomR(0, V.length v - 1)
            let (h, n, t) = partitionAt v i
            hSorted <- qsortR' h
            tSorted <- qsortR' t
            return $ hSorted <> V.cons n tSorted


-- Exercise 9 -----------------------------------------

-- Selection
select :: Ord a => Int -> Vector a -> Rnd (Maybe a)
select = undefined

-- Exercise 10 ----------------------------------------

allCards :: Deck
allCards = undefined

newDeck :: Rnd Deck
newDeck =  undefined

-- Exercise 11 ----------------------------------------

nextCard :: Deck -> Maybe (Card, Deck)
nextCard = undefined

-- Exercise 12 ----------------------------------------

getCards :: Int -> Deck -> Maybe ([Card], Deck)
getCards = undefined

-- Exercise 13 ----------------------------------------

data State = State { money :: Int, deck :: Deck }

repl :: State -> IO ()
repl s@State{..} | money <= 0  = putStrLn "You ran out of money!"
                 | V.null deck = deckEmpty
                 | otherwise   = do
  putStrLn $ "You have \ESC[32m$" ++ show money ++ "\ESC[0m"
  putStrLn "Would you like to play (y/n)?"
  cont <- getLine
  if cont == "n"
  then putStrLn $ "You left the casino with \ESC[32m$"
           ++ show money ++ "\ESC[0m"
  else play
    where deckEmpty = putStrLn $ "The deck is empty. You got \ESC[32m$"
                      ++ show money ++ "\ESC[0m"
          play = do
            putStrLn "How much do you want to bet?"
            amt <- read <$> getLine
            if amt < 1 || amt > money
            then play
            else do
              case getCards 2 deck of
                Just ([c1, c2], d) -> do
                  putStrLn $ "You got:\n" ++ show c1
                  putStrLn $ "I got:\n" ++ show c2
                  case () of
                    _ | c1 >  c2  -> repl $ State (money + amt) d
                      | c1 <  c2  -> repl $ State (money - amt) d
                      | otherwise -> war s{deck = d} amt
                _ -> deckEmpty
          war (State m d) amt = do
            putStrLn "War!"
            case getCards 6 d of
              Just ([c11, c21, c12, c22, c13, c23], d') -> do
                putStrLn $ "You got\n" ++ ([c11, c12, c13] >>= show)
                putStrLn $ "I got\n" ++ ([c21, c22, c23] >>= show)
                case () of
                  _ | c13 > c23 -> repl $ State (m + amt) d'
                    | c13 < c23 -> repl $ State (m - amt) d'
                    | otherwise -> war (State m d') amt
              _ -> deckEmpty

main :: IO ()
main = evalRandIO newDeck >>= repl . State 100
