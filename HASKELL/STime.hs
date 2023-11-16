module STime
( Time(..)
, printTime
) where

data Time = Date { day :: Int
                 , month :: Int
                 , year :: Int
                 } deriving (Show)

printTime :: Time -> String
printTime (Date day month year) = "The date is " ++ show day ++ "/"++show month ++"/" ++ show year ++ "."
