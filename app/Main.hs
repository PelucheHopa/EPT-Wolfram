module Main where

import System.Exit
import System.Environment ( getArgs )
import Utility
import Format
import Init
import Delete
import Binary
import Arg

main :: IO ()
main = do
     (x:xs) <- getArgs
     let option = Option "DISABLE" "0" "0" "0" "80" "0"
     let cellular = Cellular "111" "110" "101" "100" "011" "010" "001" "000"
     let parse = parseArgs (x:xs) option
     let move = read (optionMoveRule parse) :: Int
     let line = (read (optionLineRule parse) :: Int)
     let value = read (optionValueRule parse) :: Int
     let start = read (optionStartRule parse) :: Int
     let window = read (optionWindowRule parse) :: Int
     let addvalue = addValueBinary(initBinary value)
     let base = initBase cellular option (x:xs) addvalue
     let rogneStart = rogneDisplayStart (removeLine (removeFirst base) start) "" (line + (window `div` 2) - move + 40) (line + (window `div` 2) - move + 40) (line + 80 + start)
     let rogneEnd = rogneDisplayEnd rogneStart "" window window
     let launch = removeLinee rogneEnd line ""
     putStr launch