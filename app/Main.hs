--{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics
import Data.Foldable (traverse_)
import Data.List
import Data.Time
import Data.Maybe

data Data_csv = Data_csv {
    day_str :: String,
    second :: Int,
    --day :: Day,
    --Пока создала поля time и local_time. Потом думаю сделать посимпатичнее :)
    time :: ZonedTime,
    local_time :: LocalTime
 } deriving (Show)

instance FromRecord Data_csv where
  parseRecord v = do
    f0 <- v .! 0
    f1 <- v .! 1
    f2 <- (parseTimeM True defaultTimeLocale iso8601 f0)
    f3 <- zonedTimeToLocalTime f2
        {-Вот здесь получаю ошибку Couldn't match expected type ‘Parser LocalTime’ 
        with actual type ‘LocalTime’.
        -}
    return $ Data_csv f0 f1 f2 f3

{-valuesToList :: Data_csv -> Maybe Pomodoros
valuesToList (Data_csv a b) = case (parseTimeM True defaultTimeLocale iso8601 a :: Maybe ZonedTime) of
                              Nothing -> Nothing
                              isJust -> Just $ Pomodoros dayFromCsv b
                                        where dayFromCsv = localDay localTime
                                              localTime = zonedTimeToLocalTime (zonedTime)
                                              zonedTime = fromJust (parseTimeM True defaultTimeLocale iso8601 a :: Maybe ZonedTime)-}

listFromCsv :: BL.ByteString -> IO [Data_csv]
listFromCsv csvData = 
    case decode NoHeader csvData of
        Left err -> fail err
        Right v -> return $ V.toList v

iso8601 :: String
iso8601 = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

main :: IO ()
main = do
    csvData <- BL.readFile "pomodoros.csv"
    list <- listFromCsv csvData
    traverse_ print list

    putStrLn "End"