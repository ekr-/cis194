{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE OverloadedStrings, RecordWildCards #-}
module HW05 where

import Data.Bits
import Data.ByteString.Lazy (ByteString)
import Data.Map.Strict (Map)
import System.Environment (getArgs)

import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.Char8 as BSC
import qualified Data.Map.Strict as Map

import Parser

-- Exercise 1 -----------------------------------------

getSecret :: FilePath -> FilePath -> IO ByteString
getSecret path1 path2 = do
  f1 <- BS.readFile path1
  f2 <- BS.readFile path2
  return $ BS.filter (\x ->  x /= 0) (BS.pack $ map (\x -> xor (fst x) (snd x)) (BS.zip f1 f2)) 

decryptWithKey :: ByteString -> FilePath -> IO ()
decryptWithKey key path = do
  contents <- BS.readFile (path ++ ".enc")
  BS.writeFile path $ snd (BS.mapAccumL (\x y -> (if (x+1) == (BS.length key) then 0 else (x+1), xor (BS.index key x) y)) 0 contents)
  
-- Exercise 3 -----------------------------------------

parseFile :: FromJSON a => FilePath -> IO (Maybe a)
parseFile path = do
  contents <- BS.readFile path
  return $ decode contents

-- Exercise 4 -----------------------------------------

getBadTs :: FilePath -> FilePath -> IO (Maybe [Transaction])
getBadTs victimListPath transactionsPath = do
  victims <- parseFile victimListPath
  txs <- parseFile transactionsPath
  let vicarray = case victims of
        Just (x:xs) -> (x:xs)
        Nothing -> []

  let txsarray = case txs of
        Just (x:xs) -> (x:xs)
        Nothing -> []

  let result = case (filter (\x -> elem (tid x) vicarray) txsarray) of
        [] -> Nothing
        (x:xs) -> Just (x:xs)
  return result
  

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow [] = Map.empty
getFlow (x:xs) = Map.insertWith (+) (to x) (amount x) mp where
  mp = Map.insertWith (+) (from x) (negate $ amount x) (getFlow xs)

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal mp = fst $ Map.foldlWithKey' (\x key val -> if val > (snd x) then (key,val) else x) ("", -1) mp 

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = []

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON path txs = do
  writeFile path (encode txs)

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

