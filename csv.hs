{-# LANGUAGE OverloadedStrings #-}

import           Control.Applicative
import qualified Data.ByteString.Lazy as BL
import           Data.Csv
import qualified Data.Vector          as V

data Person = Person
    { name   :: String
    , salary :: String
    }

instance FromNamedRecord Person where
    parseNamedRecord r = Person <$> r .: "name" <*> r .: "salary"

main :: IO ()
main = do
    csvData <- BL.readFile "/Users/p3700676/Downloads/txn.csv"
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (_, v) -> V.forM_ v $ \ p ->
            putStrLn $ name p ++ " earns " ++ show (salary p) ++ " dollars"
