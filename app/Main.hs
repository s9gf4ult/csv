{-# LANGUAGE ScopedTypeVariables, DeriveGeneric #-}

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import GHC.Generics
import Data.Foldable (traverse_)
import Data.List
import Data.Time    --использую Time, а не dateTime. Ишус про импорты
import Data.Maybe
import Data.Tuple.HT

{-Помню, когда-то ты говорил, что нужно юзать не String, а Text. Здесь не стала так делать,
потому что в функции parseTimeM как раз String, а не Text. Или нужно как-то по-другому поступить?-}
data Data_csv = Data_csv String Int deriving (Generic, Show)

data Pomodoros = Pomodoros {
    day :: Day,
    sum :: Int 
} deriving (Show)

instance FromRecord Data_csv
instance ToRecord Data_csv

valuesToList :: Data_csv -> Pomodoros
valuesToList (Data_csv a b) = Pomodoros dayFromCsv b
                                where dayFromCsv = localDay localTime
                                      localTime = zonedTimeToLocalTime (fromJust zonedTime)
                                      zonedTime = (parseTimeM True defaultTimeLocale iso8601 a :: Maybe ZonedTime)
                                    
{-В этой функции не знала, как быть с Left err. В итоге сделала очень криво... В случае ошибки
возвращаю какое-то странное значение, просто для того, чтобы вернуть хоть что-то, соответствующее типу. 
Не придумала, как тут быть. Может быть, нужно использовать Maybe?-}
listFromCsv :: BL.ByteString -> [Data_csv]
listFromCsv csvData = 
    case decode NoHeader csvData of
        Left err -> [(Data_csv err 0)]
        Right v -> V.toList v

sumPomodoros :: [Pomodoros] -> Pomodoros
sumPomodoros array = Pomodoros (day (head array)) sumArray
                where sumArray = foldl (\x (Pomodoros a b) -> x+b) 0 array

equalPomodoros :: Pomodoros -> Pomodoros -> Bool
equalPomodoros (Pomodoros x y) (Pomodoros a b) = if x == a then True else False

iso8601 :: String
iso8601 = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

main :: IO ()
main = do
    csvData <- BL.readFile "pomodoros.csv"
    let list = map valuesToList (listFromCsv csvData)
    --traverse_ print list
    
    --вот, получила список дней с количеством секунд
    let groupList = map sumPomodoros (groupBy equalPomodoros list)
    traverse_ print groupList
            
    putStrLn "End"