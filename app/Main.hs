import Csv
import Prelude

main :: IO ()
main = do
    list_pomodoros <- get_pomodoros "pomodoros.csv"
    groupSortList_typeGr <- get_sort_list list_pomodoros
    let groupSortList = fst groupSortList_typeGr
    let typeGr = snd groupSortList_typeGr

    let x_max = toInteger (max_pom groupSortList)
    let inEl = countSecInEl x_max
    --putStrLn (show inEl)
    putStrLn $ printList (toInteger typeGr) inEl groupSortList

    putStrLn "End"