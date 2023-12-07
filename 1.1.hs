module Main where

import Data.Char (isDigit)
import Data.Foldable (Foldable (fold))

input :: [String]
input = ["1abc2", "pqr3stu8vwx", "a1b2c3d4e5f", "treb7uchet"]

getResult :: String -> Int
getResult s = read [head digits, last digits]
  where
    digits = filter isDigit s

main :: IO ()
-- Answer: 142
main = print . sum . map getResult $ input