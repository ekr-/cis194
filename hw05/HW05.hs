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
  victims <- (case (parseFile victimListPath) of
                Just (x:xs) -> (x:xs)
                Nothing -> [])
  return $ filter (\x -> elem (tid x) victims) (case (parseFile transactionsPath) of
                                                  Just (x:xs) -> (x:xs)
                                                  Nothing -> [])

-- Exercise 5 -----------------------------------------

getFlow :: [Transaction] -> Map String Integer
getFlow = undefined

-- Exercise 6 -----------------------------------------

getCriminal :: Map String Integer -> String
getCriminal = undefined

-- Exercise 7 -----------------------------------------

undoTs :: Map String Integer -> [TId] -> [Transaction]
undoTs = undefined

-- Exercise 8 -----------------------------------------

writeJSON :: ToJSON a => FilePath -> a -> IO ()
writeJSON = undefined

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

