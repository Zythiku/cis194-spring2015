{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import Data.Bits

import Parser
import Data.Maybe
import Data.Foldable
import Data.Ord
import Data.List

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret orig modified = do
    origFile <- BS.readFile orig
    modFile <- BS.readFile modified
    return . BS.pack . filter (/=0) $ BS.zipWith xor origFile modFile


-- Exercise 2 -----------------------------------------

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key file = do
    encFile <- BS.readFile (file ++ ".enc")
    let decFile = zipWith xor (BS.unpack encFile) (cycle $ BS.unpack key)
    BS.writeFile (file) $ BS.pack decFile

-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile filePath = do
    file <- BS.readFile filePath
    return $ decode file

-- Exercise 4 -----------------------------------------
intersectBy'             :: (a -> b -> Bool) -> [a] -> [b] -> [a]
intersectBy' _  [] _     =  []
intersectBy' _  _  []    =  []
intersectBy' eq xs ys    =  [x | x <- xs, any (eq x) ys]

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimPath transPath = do
    victims <- parseFile victimPath
    trans <- parseFile transPath 
    let badTrans = intersectBy' ((==) . tid) (fromMaybe [] trans) (fromMaybe [] victims)
    if badTrans == []
    then return $ Just badTrans
    else return $ Just badTrans

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow transactions = getFlow' Map.empty transactions
    where getFlow' m (tran:trans) = getFlow' 
                (Map.insertWith (+) (to tran) (amount tran) 
                (Map.insertWith (+) (from tran) (negate (amount tran)) m)) 
                trans
          getFlow' m _ = m


-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = fst . maximumBy (comparing snd) . Map.toList

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs m ids =
    let payers = sortBy (comparing snd) $ filter ((>0) . snd) $ Map.toList m
        payees = sortBy (comparing snd) $ filter ((<0) . snd) $ Map.toList m
    in undoTs' payers payees ids []
    where undoTs' (r:rs) (e:es) (d:ds) trans
            | (snd r) == (snd e) = undoTs' rs es ds (tran : trans)
            | (snd r) < (snd e) = undoTs' rs (updatedE : es) ds (tran : trans)
            | (snd r) > (snd e) = undoTs' (updatedR : rs) es ds (tran : trans)
            where amnt = negate $ min (snd e) (snd r)
                  tran = Transaction {from=(fst r), to=(fst e), amount=amnt, tid=d}
                  updatedE = (fst e, (snd e) - amnt)
                  updatedR = (fst r, (snd r) + amnt)
          undoTs' _ _ _ trans = trans

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON filePath a = do
    BS.writeFile filePath (encode a)

-- Exercise 9 -----------------------------------------

doEverything :: FilePath -> FilePath -> FilePath -> FilePath -> FilePath
             -> FilePath -> IO String
doEverything dog1 dog2 trans vict fids out = do
  key <- getSecret dog1 dog2
  decryptWithKey key vict
  mts <- getBadTs vict trans
  case mts of
    Nothing -> error "No Transactions"
    Just ts -> do
      mids <- parseFile fids
      case mids of
        Nothing  -> error "No ids"
        Just ids -> do
          let flow = getFlow ts       
          writeJSON out (undoTs flow ids)
          return (getCriminal flow)

main :: IO ()
main = do
  args <- getArgs
  crim <- 
    case args of
      dog1:dog2:trans:vict:ids:out:_ ->
          doEverything dog1 dog2 trans vict ids out
      _ -> doEverything "dog-original.jpg"
                        "dog.jpg"
                        "transactions.json"
                        "victims.json"
                        "new-ids.json"
                        "new-transactions.json"
  putStrLn crim

