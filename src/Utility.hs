module Utility where

{-CHECK CORRESPONDANCE-}
myVerif :: String-> String -> Bool
myVerif str tmp
     | str == tmp = True 
     | otherwise = False

{-DISPLAY NBR-}
myNumber :: String -> Bool
myNumber [c]
          | c == '0' = True
          | c == '1' = True
          | c == '2' = True
          | c == '3' = True
          | c == '4' = True
          | c == '5' = True
          | c == '6' = True
          | c == '7' = True
          | c == '8' = True
          | c == '9' = True
          | otherwise = False
myNumber (c:cs)
          | c == '0' = myNumber cs
          | c == '1' = myNumber cs
          | c == '2' = myNumber cs
          | c == '3' = myNumber cs
          | c == '4' = myNumber cs
          | c == '5' = myNumber cs
          | c == '6' = myNumber cs
          | c == '7' = myNumber cs
          | c == '8' = myNumber cs
          | c == '9' = myNumber cs
          | otherwise = False

{-CONCAT EVERYTHING-}
myConcat :: [a] -> [a] -> [a]
myConcat xs ys = foldr (:) ys xs

myAssemblyThree :: [a] -> [a] -> [a] -> [a]
myAssemblyThree a b c = foldr (:) a (foldr (:) b c)

{-COMPTE LE NBR DE LETTRE DANS LE BINAIRE (faut 8)-}
comptBinary :: String -> Int -> Int
comptBinary [xs] nbr = nbr - 1
comptBinary (x:xs) nbr = comptBinary xs (nbr - 1)