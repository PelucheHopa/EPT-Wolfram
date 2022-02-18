module Arg where
import Utility

data Option = Option String String String String String String deriving (Show)
data Cellular = Cellular String String String String String String String String deriving (Show)
data Result = Result Char Char Char Char Char Char Char Char deriving (Show)

{- GET INFORMATION -}
optionStatusRule :: Option -> String
optionStatusRule (Option rule _ _ _ _ _ ) = rule

optionValueRule :: Option -> String
optionValueRule (Option _ value _ _ _ _ ) = value

optionStartRule :: Option -> String
optionStartRule (Option _ _ start _ _ _ ) = start

optionLineRule :: Option -> String
optionLineRule (Option _ _ _ line _ _ ) = line

optionWindowRule :: Option -> String
optionWindowRule (Option _ _ _ _ window _ ) = window

optionMoveRule :: Option -> String
optionMoveRule (Option _ _ _ _ _ move ) = move

{-PARSE ET ENREGISTRE LES ARGUMENTS-}
parseArgs :: [String] -> Option -> Option
parseArgs [xs] (Option rule value start line window move) = Option rule value start line window move
parseArgs [] (Option rule value start line window move) = Option rule value start line window move
parseArgs (x:s:xs) (Option rule value start line window move)
     | myNumber s && myVerif x "--rule" = parseArgs xs (Option "ACTIVATE" s start line window move)
     | myNumber s && myVerif x "--start" = parseArgs xs (Option rule value s line window move)
     | myNumber s && myVerif x "--lines" = parseArgs xs (Option rule value start s window move)
     | myNumber s && myVerif x "--window" = parseArgs xs (Option rule value start line s move)
     | myNumber s && myVerif x "--move" = parseArgs xs (Option rule value start line window s)
     | otherwise = parseArgs (s:xs) (Option rule value start line window move)
