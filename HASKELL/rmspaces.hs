import Data.Char

removeSpaces :: String -> String
removeSpaces x = filter (isAlphaNum) x

main = interact removeSpaces
