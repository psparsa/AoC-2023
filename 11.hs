import Data.List (transpose)

zipWithIndex :: [a] -> [(Int, a)]
zipWithIndex = zip [0 ..]

data Galaxy = Position
  { x :: Int,
    y :: Int
  }

instance Show Galaxy where
  show :: Galaxy -> String
  show (Position {x, y}) =
    "{X: " ++ show x ++ ", Y: " ++ show y ++ "}"

getGalaxies :: [String] -> [Galaxy]
getGalaxies universe = concat galaxies
  where
    galaxies = map (\(i, r) -> getGalaxiesFromRow r i) (zipWithIndex universe)
    getGalaxiesFromRow row y =
      let filteredRow = filter (\(_, c) -> c == '#') (zipWithIndex row)
       in map (\(i, c) -> Position {x = i, y = y}) filteredRow

getEmptyRows :: [String] -> [Int]
getEmptyRows universe = map fst $ filter (all (== '.') . snd) (zipWithIndex universe)

getEmptyColumns :: [String] -> [Int]
getEmptyColumns = getEmptyRows . transpose

howManyPrev :: (Galaxy -> Int) -> Galaxy -> [Int] -> Int
howManyPrev accessor galaxy referenceList = length (filter (< accessor galaxy) referenceList)

howManyPrevRow :: Galaxy -> [Int] -> Int
howManyPrevRow = howManyPrev y

howManyPrevCol :: Galaxy -> [Int] -> Int
howManyPrevCol = howManyPrev x

addSpaceBetweenGalaxies :: [Galaxy] -> [Int] -> [Int] -> Int -> [Galaxy]
addSpaceBetweenGalaxies galaxies emptyRows emptyColumns multiplier = do
  let increasedY = map (\pos -> pos {y = y pos + howManyPrevRow pos emptyRows * (multiplier - 1)}) galaxies
  let increasedX = map (\pos -> pos {x = x pos + howManyPrevCol pos emptyColumns * (multiplier - 1)}) increasedY
  increasedX

getDistance :: Galaxy -> Galaxy -> Int
getDistance galaxyA galaxyB = do
  let a = abs (x galaxyA - x galaxyB)
  let b = abs (y galaxyA - y galaxyB)
  a + b

calculateDistances :: [Galaxy] -> [Int]
calculateDistances galaxies =
  [ getDistance galaxyA galaxyB
    | (i, galaxyA) <- zipWithIndex galaxies,
      (j, galaxyB) <- zipWithIndex (drop (i + 1) galaxies)
  ]

main :: IO ()
main = do
  content <- readFile "./11.input.txt"
  let universe = lines content
  let galaxies = getGalaxies universe
  let emptyRows = getEmptyRows universe
  let emptyColumns = getEmptyColumns universe
  let getGalaxiesWithAddedSpace = addSpaceBetweenGalaxies galaxies emptyRows emptyColumns
  let getAnswer = sum . calculateDistances . getGalaxiesWithAddedSpace
  let answer1 = getAnswer 2
  let answer2 = getAnswer 1_000_000
  putStrLn ("Answer 1: " ++ show answer1) -- Answer: 374
  putStrLn ("Answer 2: " ++ show answer2) -- Answer: 82000210
