{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( parseCsv
    ) where

import           Control.Applicative        ()
import qualified Data.ByteString.Lazy.Char8 as BL
import           Data.Csv                   (FromNamedRecord (..),
                                             HasHeader (NoHeader), Header,
                                             decode, decodeByName, header, (.:))
import           Data.Either                (fromRight)
import           Data.Time
import qualified Data.Vector                as V
import  qualified Data.Maybe as MB

data UsageRecord = UsageRecord {
   usageType :: String
,  date      :: String
,  startTime :: String
,  endTime   :: String
,  usage     :: Float
,  units     :: String
,  cost      :: String
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
            <*> r .: "COST"

getCsvDataFromCsv :: Either String (Header, V.Vector UsageRecord) -> (Header, V.Vector UsageRecord)
getCsvDataFromCsv = fromRight (header [], V.empty)


getTotalUsages :: V.Vector UsageRecord -> Integer
getTotalUsages usages = 0

parseTimeString :: String -> Maybe TimeOfDay
parseTimeString =
    parseTimeM True defaultTimeLocale "%H:%M"

parseDateString :: String -> Day
parseDateString =
    let date = parseTimeM True defaultTimeLocale "%F" :: Maybe Day

    MB.fromMaybe (fromGregorian 1970 1 1) date

isElectricChargingUsage :: UsageRecord -> Bool
isElectricChargingUsage record =
    True

printRecord :: UsageRecord -> IO ()
printRecord p =
   putStrLn $ usageType p ++ date p ++ startTime p ++ endTime p ++ show (usage p) ++ units p ++ cost p

parseCsv :: String -> IO ()
parseCsv fileName = do
    csvData <- BL.readFile fileName
    let decoded = decodeByName csvData
    let (_, records) = getCsvDataFromCsv decoded

    putStrLn "Filtered"

    let filteredRecords = V.filter isElectricChargingUsage records
    V.forM_ filteredRecords printRecord

    putStrLn "Date"

    V.forM_ filteredRecords (\r -> putStrLn parseDateString (date r))



    -- case decoded of
    --     Left err -> putStrLn err
    --     Right (h, v) ->
    --         V.forM_ v printRecord
