{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import           Data.Function
import           Data.List
import           Data.Maybe
import           Data.Text
import qualified Data.Vector          as V
import           System.Locale
import           System.Time.Parse

txn :: Text
txn = pack "12 august 2016,Retragere numerar,\"100,00\",\
\,Nr. card: 52xx xxxx xxxx 7419,,\
\,Terminal: 6132 ING TM IULIUS MALL  RO  TIMISOARA,,\
\,Data: 12-08-2016,,\
\,Retragere numerar,\"400,00\",\
\,Nr. card: 42xx xxxx xxxx 6660,,\
\,Terminal: 5545 ING OFFICE TM REPUBL  RO  TIMIS,,\
\,Data: 11-08-2016,,\
\11 august 2016,Retragere numerar,\"200,00\",\
\,Nr. card: 52xx xxxx xxxx 7419,,\
\,Terminal: 6132 ING TM IULIUS MALL  RO  TIMISOARA,,\
\,Data: 11-08-2016,,\
\10 august 2016,Cumparare POS,\"7,00\",\
\,Nr. card: 42xx xxxx xxxx 6660,,\
\,Terminal: CINEMA CITY TIMISOARA DEP  RO  TIMISOARA,,\
\,Data: 07-08-2016 Autorizare: 036064,,"


data Txn = Txn
  {  txndate   :: String
   , txndetail :: String
   , debit     :: String
   , credit    :: String
  }

instance FromNamedRecord Txn where
    parseNamedRecord r = Txn <$> r .: "txndate" <*> r .: "txndetail" <*> r .: "debit" <*> r .: "credit"

somme::String -> Bool
somme str = Data.Maybe.isNothing (parseCalendarTime System.Locale.defaultTimeLocale "%d %B %Y" str)

rep = replace

lst :: Text -> [[Text]]
lst (x:xs) =  if some x then x:lst xs else [lst x]

main = do
  -- print $ Data.List.groupBy ((==) `on` some) txn
  print $ lst $ Data.Text.splitOn "," txn

main' :: IO ()
main' = do
   csvData <- BL.readFile "/Users/p3700676/Downloads/txn.csv"
   case decodeByName csvData of
       Left err -> putStrLn err
       Right (_, v) -> V.forM_ v $ \ p ->
           putStrLn $ txndate p
