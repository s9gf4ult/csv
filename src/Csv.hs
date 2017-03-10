module Csv where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import qualified Data.Vector as V
import Data.List
import Data.Time
import Data.Maybe
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import Data.Time.Clock

import System.IO.Unsafe

--данные из файла .csv
data Data_csv = Data_csv {
    day_str :: String,
    second :: Int,
    zoned_time :: ZonedTime
 } deriving (Show)

data Pomodoros = Pomodoros {
    day :: Day,
    sum_sec :: Int,
    day_week :: Int,
    time_of_day :: TimeOfDay,
    hour :: Int,
    part_day :: PartOfDay
} deriving (Show)

--data PartOfDay = Morning | Afternoon | Evening | Night
data PartOfDay = Morning_9_12 | Afternoon_12_17 | Evening_17_24 | Night_24_9
     deriving (Show, Eq, Ord)

instance FromRecord Data_csv where
  parseRecord v = do
    f0 <- v .! 0
    f1 <- v .! 1
    f2 <- (parseTimeM True defaultTimeLocale iso8601 f0)
    return $ Data_csv f0 f1 f2

listFromCsv :: BL.ByteString -> IO [Data_csv]
listFromCsv csvData = 
    case decode NoHeader csvData of
        Left err -> fail (err ++ " !!!")
        Right v -> return $ V.toList v

calcPartOfDay :: Int -> PartOfDay
calcPartOfDay h = if (h >= 9) && (h < 12) then Morning_9_12
                  else if (h >= 12) && (h < 17) then Afternoon_12_17
                       else if (h >= 17) && (h < 24) then Evening_17_24
                            else Night_24_9
                  
valuesToList :: Data_csv -> Maybe Pomodoros
valuesToList (Data_csv a b c) = Just $ Pomodoros dayFromCsv b day_week time_of_day_zone hour part_day
        where dayFromCsv = localDay localTime
              localTime = zonedTimeToLocalTime c
              day_week = snd $ mondayStartWeek dayFromCsv
              hour = todHour time_of_day_zone
              time_of_day = timeToTimeOfDay diff_time
              diff_time = utctDayTime utc_time
              utc_time = zonedTimeToUTC c
              x = unsafePerformIO (getTimeZone utc_time)
              time_of_day_zone = snd $ utcToLocalTimeOfDay x time_of_day
              part_day = calcPartOfDay hour

sumPomodoros :: Int -> [Pomodoros] -> Pomodoros
sumPomodoros type_gr array = case type_gr of
    1 -> Pomodoros (day (head array)) sum_sec (day_week (head array)) midday 0 (calcPartOfDay 0)   --по дням
            where sum_sec = foldl (\x (Pomodoros a b c d e f) -> x+b) 0 array
    2 -> Pomodoros (fromGregorian 0 0 0) sumArray (day_week (head array)) midday 0 (calcPartOfDay 0)    --по дням недели
            where sumArray = foldl (\x (Pomodoros a b c d e f) -> x+b) 0 array
    3 -> Pomodoros (fromGregorian 0 0 0) sumArray 0 midday (hour (head array)) (calcPartOfDay (hour (head array)))    --по часам
            where sumArray = foldl (\x (Pomodoros a b c d e f) -> x+b) 0 array
    4 -> Pomodoros (fromGregorian 0 0 0) sumArray 0 midday (hour (head array)) (calcPartOfDay (hour (head array)))    --по части суток
            where sumArray = foldl (\x (Pomodoros a b c d e f) -> x+b) 0 array

equalPomodoros :: Int -> Pomodoros -> Pomodoros -> Bool
equalPomodoros type_gr p1 p2 = case type_gr of
       1 -> if (day p1) == (day p2) then True else False     --группировка по дням
       2 -> if (day_week p1) == (day_week p2) then True else False     --по дням недели
       3 -> if (hour p1) == (hour p2) then True else False     --по часам
       4 -> if (part_day p1) == (part_day p2) then True else False      --часть суток

iso8601 :: String
iso8601 = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

sortGT :: Int -> Pomodoros -> Pomodoros -> Ordering
sortGT type_gr p1 p2 = case type_gr of
    1 -> case (day p1) > (day p2) of
            True -> GT
            otherwise -> LT
    2 -> case (day_week p1) > (day_week p2) of
            True -> GT
            otherwise -> LT
    3 -> case (hour p1) > (hour p2) of
            True -> GT
            otherwise -> LT
    4 -> case (part_day p1) > (part_day p2) of
            True -> GT
            otherwise -> LT

--не использую groupBy
group_pom :: Int -> [Pomodoros] -> [[Pomodoros]]
group_pom type_gr x = if null x_no
                      then [x_yes]
                      else [x_yes] ++ (group_pom type_gr x_no)
                      where p = partition (equalPomodoros type_gr (head x)) x
                            x_yes = fst p
                            x_no = snd p
                            res = (group_pom type_gr x_no)

printList :: Integer -> Integer -> [Pomodoros] -> String
printList _ _ [] = ""
printList typeGr inEl (x:xs) = printRecord typeGr inEl x ++ "\n" ++ printList typeGr inEl xs

printRecord :: Integer -> Integer -> Pomodoros -> String
printRecord type_gr inEl p = case type_gr of
   --по дням
   1 -> "day = " ++ show (day p)
        -- ++ "  sum_sec = " ++ show (sum_sec p)
        ++ "  minute = " ++ show (div (sum_sec p) 60)
        ++ "\t" ++ grafPomodoros (toInteger (sum_sec p)) inEl
   --по дням недели
   2 -> "day_week = " ++ show (day_week p) 
        -- ++ "  sum_sec = " ++ show (sum_sec p)
        ++ "  minute = " ++ show (div (sum_sec p) 60)
        ++ "\t" ++ grafPomodoros (toInteger (sum_sec p)) inEl
   --по часам
   3 -> "hour = " ++ show (hour p) 
        -- ++ "  sum_sec = " ++ show (sum_sec p)
        ++ "  minute = " ++ show (div (sum_sec p) 60)
        ++ "\t" ++ grafPomodoros (toInteger (sum_sec p)) inEl
   --часть суток
   4 -> "part_day = " ++ show (part_day p) 
        -- ++ "  sum_sec = " ++ show (sum_sec p)
        ++ "  minute = " ++ show (div (sum_sec p) 60)
        ++ "\t" ++ grafPomodoros (toInteger (sum_sec p)) inEl

grafPomodoros :: Integer -> Integer -> String
grafPomodoros sec inEl = str (div sec inEl)

str :: Integer -> String
str 0 = ""
str count = "|" ++ str (count-1)

max_pom :: [Pomodoros] -> Int
max_pom [] = 0
max_pom x = max (sum_sec $ head x) (max_pom (tail x))

min_pom :: [Pomodoros] -> Int
min_pom [] = maxBound :: Int
min_pom x = min (sum_sec $ head x) (min_pom (tail x))

--максимальное количество символов '|' в одной строке
countEl :: Integer
countEl = 40

--количество секунд в одном символе '|'
countSecInEl :: Integer -> Integer
countSecInEl x_max = div x_max countEl

nothing_bool :: [Maybe Pomodoros] -> Bool
nothing_bool [] = False
nothing_bool (x:xs) = if (isNothing x) then True else (nothing_bool xs)

get_pomodoros :: String -> IO [Pomodoros]
get_pomodoros str = do
        csvData <- BL.readFile str
        ar <- listFromCsv csvData
        let list = map valuesToList ar

        --Из 7го ишуса - программа должна честно сообщать об ошибках, чтобы пользователь мог принять меры
        if nothing_bool list then putStrLn "function valuesToList - error ZonedTime" else putStr ""    
        let list_pomodoros = catMaybes $ list
        return list_pomodoros

--тип группировки тоже возвращаю, чтобы в Main соответствующе распечатать
get_sort_list :: [Pomodoros] -> IO ([Pomodoros], Int)
get_sort_list list_pomodoros = do
        putStrLn "1 - группировка по дням"
        putStrLn "2 - группировка по дням недели"
        putStrLn "3 - группировка по часам дня"
        putStrLn "4 - группировка по части суток"
        type_gr_str <- getLine
        let type_gr = read type_gr_str
        if elem type_gr [1,2,3,4] then do
                --сначала группирую
                let groupList = map (sumPomodoros type_gr) (group_pom type_gr list_pomodoros)
                --потом сортирую
                let groupSortList = (sortBy (sortGT type_gr) groupList)
                return (groupSortList, type_gr)
        else do
                --putStrLn "Нет такого типа группировки"
                return ([], type_gr)