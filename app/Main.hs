import Csv
import Prelude

main :: IO ()
main = do
    list_pomodoros <- get_pomodoros "pomodoros.csv"
    grouped_pomodoros <- get_sort_list list_pomodoros
    print_grouped_pomodoros grouped_pomodoros
    putStrLn "End"
