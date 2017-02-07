module Main where

import Data.Foldable
import Data.Strings
import Graphics.UI.Gtk
import Lib
import System.Environment




import qualified Data.DateTime as DT
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Text.Internal as T

func :: T.Text -> (T.Text, T.Text, DT.DateTime)
func str = (zFilter !! 0, zFilter !! 1, DT.fromSeconds (read (T.unpack (zFilter !! 0))) )
            where zFilter = filter (\x -> if x /= T.pack (",") then True else False) z
                  z = T.groupBy (\x y -> if (x /= ',' && y /= ',') then True else False) str

doArray :: [T.Text] -> [(T.Text, T.Text, DT.DateTime)]
doArray a = map func a

unpack' :: (T.Text, T.Text, DT.DateTime) -> (String, String, String, (Integer, Int, Int, Int, Int, Int))
unpack' (x, y, z) = (T.unpack x, T.unpack y, show z, DT.toGregorian z)

main = do
    contents <- T.readFile "pomodoros.csv"
    let todoTasks = T.lines contents
        res = doArray todoTasks
        x = map unpack' res

    {-initGUI
    window <- windowNew
    button <- buttonNew
    set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
        containerChild := button, containerBorderWidth := 30]
    onClicked button (hello button)
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI-}

    traverse_ print x
    putStrLn "End"

{-main = do
    getArgsAndInitialize
    createAWindow "Red Window"
    mainLoop

createAWindow windowName = do
    createWindow windowName
    displayCallback $= display

display = do
    currentColor $= Color4 0 0 0 1
    clear [ColorBuffer]

    flush-}
