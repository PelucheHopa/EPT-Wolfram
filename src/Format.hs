module Format where
import Utility

rogneDisplayStart :: String -> String -> Int -> Int -> Int -> String
rogneDisplayStart [xs] str nbr base source = myConcat str "\n"
rogneDisplayStart (x:xs) str nbr base source
     | nbr > 0 = rogneDisplayStart xs str (nbr - 1) base source
     | nbr == 0 && x /= '\n' = rogneDisplayStart xs (myConcat str [x]) nbr base source
     | nbr == 0 && x == '\n' = rogneDisplayStart xs (myConcat str [x]) base base source

rogneDisplayEnd :: String -> String -> Int -> Int -> String
rogneDisplayEnd [xs] str nbr base = myConcat str "\n"
rogneDisplayEnd (x:xs) str nbr base
     | nbr > 0 = rogneDisplayEnd xs (myConcat str [x]) (nbr - 1) base
     | nbr == 0 && x /= '\n' = rogneDisplayEnd xs str nbr base
     | nbr == 0 && x == '\n' = rogneDisplayEnd xs (myConcat str [x]) base base