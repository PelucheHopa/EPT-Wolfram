module Delete where
import Utility

{- retire la premiere lettre de la string -}
removeFirst :: String -> String
removeFirst (x:xs) = xs

{-retire un nbr de ligne X-}
removeLine :: String -> Int -> String
removeLine [xs] nbr = [xs]
removeLine (x:xs) nbr
     | x /= '\n' && nbr > 0 = removeLine xs nbr
     | nbr <= 0 = x:xs
     | otherwise = removeLine xs (nbr - 1)

removeLinee :: String -> Int -> String -> String
removeLinee [xs] nbr str = (myConcat str [xs])
removeLinee (x:xs) nbr str
     | x == '\n' && nbr > 0 = removeLinee xs (nbr - 1) (myConcat str [x])
     | nbr == 0 = str
     | otherwise = removeLinee xs nbr (myConcat str [x])