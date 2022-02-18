module Init where
import Utility
import Arg
import Binary

{-CREER LA STRING DE BASE-}
stringBase :: Int -> String -> String
stringBase nbr str
     | nbr == 0 = myConcat str "*"
     | nbr > 0 = stringBase (nbr - 1) (myConcat str " ")
     
stringEnd :: Int -> String -> String
stringEnd nbr str
     | nbr == 0 = str
     | nbr > 0 = stringEnd (nbr - 1) (myConcat str  " ")

initEnding :: Cellular -> Option -> String -> String -> String -> String
initEnding cellular option binary [xs] (a:as) = myConcat "" (a:as) {-(newCharBinary (myAssemblyThree [xs] " " " ") binary) -}
initEnding cellular option binary (x:xs) (a:as) = myConcat (a:as) (initEnding cellular option binary xs (newCharBinary (x:xs) binary))

initBeginning :: Cellular -> Option -> String -> String -> Int -> String
initBeginning cellular option binary (x:xs) nbr
     | nbr > 1 = myConcat "\n" (myConcat (x:xs) (initBeginning cellular option binary (initEnding cellular option binary (x:xs) (newCharBinary (myConcat " " (x:xs)) binary)) (nbr - 1)))
     | otherwise = myConcat (myConcat "\n" (x:xs)) "\n"

initBase :: Cellular -> Option -> [String] -> String -> String
initBase cellular option args binary = initBeginning cellular option binary (stringEnd ((read (optionLineRule(parseArgs args option)) :: Int) + 40 + (read (optionWindowRule(parseArgs args option)) :: Int)) (stringBase ((read (optionLineRule(parseArgs args option)) :: Int) + 40 + (read (optionWindowRule(parseArgs args option)) :: Int)) "")) ((read (optionLineRule(parseArgs args option)) :: Int) + 40 + (read (optionStartRule(parseArgs args option)) :: Int))