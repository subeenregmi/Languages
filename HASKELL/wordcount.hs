lengthOfLine :: String -> String
lengthOfLine x = show $ length . words $ x

main = interact lengthOfLine
