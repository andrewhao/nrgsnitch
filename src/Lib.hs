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
import           Data.Maybe                 (fromMaybe)
import           Data.Time                  (Day, TimeOfDay (todHour),
                                             defaultTimeLocale, formatTime,
                                             fromGregorian, midnight,
                                             parseTimeM)
import qualified Data.Vector                as V
import           Text.Regex.PCRE            (AllTextMatches (getAllTextMatches),
                                             (=~))

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

parseTimeString :: String -> TimeOfDay
parseTimeString time =
    let t = parseTimeM True defaultTimeLocale "%H:%M" time
    in fromMaybe midnight t

parseDateString :: String -> Day
parseDateString ds =
    let d = parseTimeM True defaultTimeLocale "%F" ds
    in fromMaybe (fromGregorian 1970 1 1) d

printDay :: Day -> IO ()
printDay day = putStrLn (formatTime defaultTimeLocale "%D" day)

printTimeOfDay :: TimeOfDay -> IO ()
printTimeOfDay time = putStrLn (formatTime defaultTimeLocale "%T" time)

isElectricChargingUsage :: UsageRecord -> Bool
isElectricChargingUsage record =
    todHour (parseTimeString . startTime $ record) < 8

parseUsageAmountCents :: String -> Float
parseUsageAmountCents amountStr =
    let matchExpr = (amountStr =~ ("[0-9.]+" :: String)) :: AllTextMatches [] String
        matchGroups = getAllTextMatches matchExpr
    in read (head matchGroups) :: Float

usageAmountCents :: UsageRecord -> Float
usageAmountCents = parseUsageAmountCents . cost

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

    V.forM_ filteredRecords (printDay . parseDateString . date)

    putStrLn "Time"

    V.forM_ filteredRecords (printTimeOfDay . parseTimeString . startTime)

    putStrLn "Amount"

    V.forM_ filteredRecords (print . usageAmountCents)

    putStrLn "Total Amount"

    let usageEntries = (V.sum . V.map usageAmountCents) filteredRecords
    print usageEntries
