module Binary where
import Utility

{-AJOUTE L'ESPACE MANQUANT AU BINAIRE-}
addValueBinary :: String -> String
addValueBinary (x:xs)
     | comptBinary (x:xs) 8 > 0 = addValueBinary (myConcat " " (x:xs))
     | otherwise = x:xs

{-CREATE BINARY NUMBER-}
initBinary :: Int -> String
initBinary nbr
     | nbr == 1 = "*"
     | nbr == 0 = " "
     | mod nbr 2 == 1 = myConcat (initBinary (div (nbr - 1)  2)) "*"
     | otherwise = myConcat (initBinary (div nbr 2)) " "

{-PREND LE CHIFFRE ASSIGNER AU BINAIRE-}
takeBinary :: Int -> String -> String
takeBinary nbr [xs]
     | nbr == 1 = [xs]
     | otherwise = ""
takeBinary nbr (x:xs)
     | nbr == 1 = [x]
     | otherwise = takeBinary (nbr - 1) xs

{-CHECK LA CORRESPONDANCE-}
newCharBinary :: String -> String -> String
newCharBinary [xs] binary = "NUL"
newCharBinary [] binary = "NUL2"
newCharBinary (a:b:'\n':xs) binary
     | myAssemblyThree [a] [b] " " == "***" = takeBinary 1 binary
     | myAssemblyThree [a] [b] " " == "** " = takeBinary 2 binary
     | myAssemblyThree [a] [b] " " == "* *" = takeBinary 3 binary
     | myAssemblyThree [a] [b] " " == "*  " = takeBinary 4 binary
     | myAssemblyThree [a] [b] " " == " **" = takeBinary 5 binary
     | myAssemblyThree [a] [b] " " == " * " = takeBinary 6 binary
     | myAssemblyThree [a] [b] " " == "  *" = takeBinary 7 binary
     | myAssemblyThree [a] [b] " " == "   " = takeBinary 8 binary
newCharBinary (a:b:c:xs) binary
     | myAssemblyThree [a] [b] [c] == "***" = takeBinary 1 binary
     | myAssemblyThree [a] [b] [c] == "** " = takeBinary 5 binary
     | myAssemblyThree [a] [b] [c] == "**\n" = takeBinary 5 binary
     | myAssemblyThree [a] [b] [c] == "* *" = takeBinary 3 binary
     | myAssemblyThree [a] [b] [c] == "*\n*" = takeBinary 3 binary
     | myAssemblyThree [a] [b] [c] == "*  " = takeBinary 7 binary
     | myAssemblyThree [a] [b] [c] == "*\n " = takeBinary 7 binary
     | myAssemblyThree [a] [b] [c] == "* \n" = takeBinary 7 binary
     | myAssemblyThree [a] [b] [c] == " **" = takeBinary 2 binary
     | myAssemblyThree [a] [b] [c] == "\n**" = takeBinary 2 binary
     | myAssemblyThree [a] [b] [c] == " * " = takeBinary 6 binary
     | myAssemblyThree [a] [b] [c] == "\n* " = takeBinary 6 binary
     | myAssemblyThree [a] [b] [c] == " *\n" = takeBinary 6 binary
     | myAssemblyThree [a] [b] [c] == "  *" = takeBinary 4 binary
     | myAssemblyThree [a] [b] [c] == " \n*" = takeBinary 4 binary
     | myAssemblyThree [a] [b] [c] == "\n *" = takeBinary 4 binary
     | myAssemblyThree [a] [b] [c] == "   " = takeBinary 8 binary
     | myAssemblyThree [a] [b] [c] == "\n  " = takeBinary 8 binary
     | myAssemblyThree [a] [b] [c] == "  \n" = takeBinary 8 binary
     | myAssemblyThree [a] [b] [c] == " \n " = takeBinary 8 binary
newCharBinary (a:b:xs) binary
     | myAssemblyThree [a] [b] " " == "***" = takeBinary 1 binary
     | myAssemblyThree [a] [b] " " == "** " = takeBinary 5 binary
     | myAssemblyThree [a] [b] " " == "**\n" = takeBinary 5 binary
     | myAssemblyThree [a] [b] " " == "* *" = takeBinary 3 binary
     | myAssemblyThree [a] [b] " " == "*  " = takeBinary 7 binary
     | myAssemblyThree [a] [b] " " == "* \n" = takeBinary 7 binary
     | myAssemblyThree [a] [b] " " == " **" = takeBinary 2 binary
     | myAssemblyThree [a] [b] " " == "\n**" = takeBinary 2 binary
     | myAssemblyThree [a] [b] " " == " * " = takeBinary 6 binary
     | myAssemblyThree [a] [b] " " == "\n* " = takeBinary 6 binary
     | myAssemblyThree [a] [b] " " == " *\n" = takeBinary 6 binary
     | myAssemblyThree [a] [b] " " == "  *" = takeBinary 4 binary
     | myAssemblyThree [a] [b] " " == "\n *" = takeBinary 4 binary
     | myAssemblyThree [a] [b] " " == "   " = takeBinary 8 binary
     | myAssemblyThree [a] [b] " " == "\n  " = takeBinary 8 binary
     | myAssemblyThree [a] [b] " " == "  \n" = takeBinary 8 binary