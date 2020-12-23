{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseCsv
    ) where

import           Control.Applicative        ()
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv                   (FromNamedRecord (..),
                                             HasHeader (NoHeader), decode,
                                             decodeByName, (.:))
import qualified Data.Vector                as V

data UsageRecord = UsageRecord {
   usageType :: String
,  date      :: String
,  startTime :: String
,  endTime   :: String
,  usage     :: Float
,  units     :: String
} deriving (Show)

instance FromNamedRecord UsageRecord where
    parseNamedRecord r =
        UsageRecord
            <$> r .: "TYPE"
            <*> r .: "DATE"
            <*> r .: "START TIME"
            <*> r .: "END TIME"
            <*> r .: "USAGE"
            <*> r .: "UNITS"



parseCsv :: String -> IO ()
parseCsv fileName = do
    csvData <- BL.readFile fileName
    case decodeByName csvData of
        Left err -> putStrLn err
        Right (h, v) ->
            V.forM_ v $ \ p ->
                putStrLn $ usageType p ++ date p ++ startTime p ++ endTime p ++ show (usage p) ++ units p
