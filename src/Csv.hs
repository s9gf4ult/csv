module Csv
  ( get_pomodoros
  , get_sort_list
  , print_grouped_pomodoros
  ) where

import qualified Data.ByteString.Lazy as BL
import Data.Csv
import Data.Foldable
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.Ratio
import Data.Time
import Data.Time.Calendar.OrdinalDate (mondayStartWeek)
import qualified Data.Vector as V

import System.IO.Unsafe

--данные из файла .csv
data Data_csv = Data_csv {
    day_str :: String,
    second :: Int,
    zoned_time :: ZonedTime
 } deriving (Show)


instance FromRecord Data_csv where
  parseRecord v = do
    f0 <- v .! 0
    f1 <- v .! 1
    f2 <- (parseTimeM True defaultTimeLocale iso8601 f0)
    return $ Data_csv f0 f1 f2

-- После сделанный преобразований данный тип не особо нужен, ведь мы
-- печатаем только секунды, а группируем по значениям, которые можно
-- вывести из 'Data_csv'
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

data GroupedPomodoros
  = ByDay (Map Day [Pomodoros])
  | ByWeekDay (Map Int [Pomodoros]) -- Тут ключ - день недели, число от 1 до 7
  | ByHour (Map Int [Pomodoros]) -- Тут ключ - час внутри дня
  | ByPartOfDay (Map PartOfDay [Pomodoros])
  deriving (Show)

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

iso8601 :: String
iso8601 = iso8601DateFormat $ Just "%H:%M:%S%Q%z"

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

-- Тут и дальше группировка помидоров

--тип группировки тоже возвращаю, чтобы в Main соответствующе распечатать
get_sort_list :: [Pomodoros] -> IO GroupedPomodoros
get_sort_list list_pomodoros = do
        putStrLn "1 - группировка по дням"
        putStrLn "2 - группировка по дням недели"
        putStrLn "3 - группировка по часам дня"
        putStrLn "4 - группировка по части суток"
        type_gr_str <- getLine
        case type_gr_str of
          "1" -> return $ groupByDay list_pomodoros
          "2" -> return $ groupByWeek list_pomodoros
          "3" -> return $ groupByHour list_pomodoros
          "4" -> return $ groupByPartOfDay list_pomodoros
          _   -> get_sort_list list_pomodoros
            -- Запустить вопрос повторно, если пользователь ошибся

groupByDay :: [Pomodoros] -> GroupedPomodoros
groupByDay poms = ByDay $ M.fromListWith (++) $ map go poms
  where
    go :: Pomodoros -> (Day, [Pomodoros])
    go pom = (day pom, [pom])

groupByWeek :: [Pomodoros] -> GroupedPomodoros
groupByWeek poms = ByWeekDay $ M.fromListWith (++) $ map go poms
  where
    go :: Pomodoros -> (Int, [Pomodoros])
    go pom = (snd $ mondayStartWeek $ day pom, [pom])

groupByHour :: [Pomodoros] -> GroupedPomodoros
groupByHour poms = ByHour $ M.fromListWith (++) $ map go poms
  where
    go :: Pomodoros -> (Int, [Pomodoros])
    go pom = (todHour $ time_of_day pom, [pom])

groupByPartOfDay :: [Pomodoros] -> GroupedPomodoros
groupByPartOfDay poms = ByPartOfDay $ M.fromListWith (++) $ map go poms
  where
    go :: Pomodoros -> (PartOfDay, [Pomodoros])
    go pom = (part_day pom, [pom])


-- Тут и ниже функции для вывода на печать
print_grouped_pomodoros :: GroupedPomodoros -> IO ()
print_grouped_pomodoros gpoms = case gpoms of
  ByDay m -> do
    let
      daysList :: [(Day, [Pomodoros])]
      daysList = M.toAscList m
        -- Тут мы получаем сортированные по дням список пар. Так как в
        -- словаре ключи хранятся в уже сортированном виде
      sums :: [(Day, Int)]
      sums = summPomodors daysList
      maxLen = maximum $ map snd sums
    for_ sums $ \(day, secs) -> do
      let str = showDay day ++ showSecs maxLen secs
      putStrLn str
  ByWeekDay m -> do
    let
      weeksList :: [(Int, [Pomodoros])]
      weeksList = M.toAscList m
      sums = summPomodors weeksList
      maxLen = maximum $ map snd sums
    for_ sums $ \(weekDay, secs) -> do
      let str = showWeek weekDay ++ showSecs maxLen secs
      putStrLn str
  ByHour m -> do
    let
      hoursList = M.toAscList m
      sums = summPomodors hoursList
      maxLen = maximum $ map snd sums
    for_ sums $ \(hour, secs) -> do
      putStrLn $ showHour hour ++ showSecs maxLen secs
  ByPartOfDay m -> do
    let
      partsList = M.toAscList m
      sums = summPomodors partsList
      maxLen = maximum $ map snd sums
    for_ sums $ \(pd, secs) -> do
      putStrLn $ showPartDay pd ++ showSecs maxLen secs

summPomodors :: [(a, [Pomodoros])] -> [(a, Int)]
summPomodors a = map go a
  where
    go (a, poms) = (a, sum $ map sum_sec poms)

normalizeLen :: String -> String
normalizeLen s = take l $ s ++ spaces
  where
    spaces = replicate (max 0 $ l - length s) ' '
    l = 15 -- Длинна строки, к которой подогнать данную строку

showDay :: Day -> String
showDay d = (normalizeLen $ show d) ++ ": "

showSecs :: Int -> Int -> String
showSecs maxLen secs = replicate bars '|'
  where
    bars = floor $ maxBars * (secs % maxLen)
    -- Тут я использовал небольшой трюк: оператор (%) на самом деле
    -- конструироует рациональное число с типом `Ratio Int`, которое
    -- домножается на 50 (тоже того же типа, который выводится
    -- неявно), затем округляю полученное рациональное число до
    -- целого. Таким образом, любое целое число от 0 до maxLen будет
    -- нормализовано до целого числа от 0 до 50
    maxBars = 50

showWeek :: Int -> String
showWeek wd = (normalizeLen $ show wd) ++ ": "

showHour :: Int -> String
showHour h = (normalizeLen $ show h) ++ ": "

showPartDay :: PartOfDay -> String
showPartDay pd = (normalizeLen $ show pd) ++ ": "
