module Main where

import Lib
import System.Environment
import Data.Strings
import Graphics.UI.Gtk
--import Graphics.UI.GLUT

func :: String -> (String, String)
func str = (head (strSplitAll "," str), head (tail (strSplitAll "," str)))

doArray :: [String] -> [(String, String)]
doArray a = map func a

--hello :: (ButtonClass o) => o -> IO ()
--hello b = set b [buttonLabel := "Hello World"]

main = do
    contents <- readFile "pomodoros.csv"
    let todoTasks = lines contents
        res = doArray todoTasks

{-    initGUI
    window <- windowNew
    button <- buttonNew
    set window [windowDefaultWidth := 200, windowDefaultHeight := 200,
        containerChild := button, containerBorderWidth := 30]
    onClicked button (hello button)
    onDestroy window mainQuit
    widgetShowAll window
    mainGUI-}
    print $ res
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