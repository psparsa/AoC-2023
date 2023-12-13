{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Avoid lambda" #-}
{-# HLINT ignore "Redundant bracket" #-}
import Data.List (find, groupBy, transpose)
import Data.Maybe (fromJust, fromMaybe, isJust)

type Pattern = [String]

getRowDiff :: Int -> String -> Int
getRowDiff splitPoint row = do
  let (leftSide, righSide) = splitAt splitPoint row
  length (filter not (zipWith (==) (reverse leftSide) righSide))

findReflection :: Int -> Pattern -> Maybe Int
findReflection maximumAllowedDiff grid = find (\pos -> x pos == maximumAllowedDiff) [1 .. length (head grid) - 1]
  where
    x splitPoint = sum (map (\row -> getRowDiff splitPoint row) grid)

getAnswer :: Int -> [Pattern] -> Int
getAnswer maximumAllowedDiff grids =
  sum
    ( map
        ( \grid -> do
            let columnReflection = findReflection maximumAllowedDiff grid
            let rowReflection = findReflection maximumAllowedDiff (transpose grid)
            (fromMaybe 0 columnReflection) + ((fromMaybe 0 rowReflection) * 100)
        )
        grids
    )

answer1 :: [Pattern] -> Int
answer1 = getAnswer 0

answer2 :: [Pattern] -> Int
answer2 = getAnswer 1

main :: IO ()
main = do
  contents <- readFile "./13.input.txt"
  let patterns = filter (\x -> length x > 1) (groupBy (\a b -> not (b == "" || a == "")) (lines contents)) :: [Pattern]
  putStr "Answer 1: "
  print (answer1 patterns) -- Answer: 405
  putStr "Answer 2: "
  print (answer2 patterns) -- Answer: 400