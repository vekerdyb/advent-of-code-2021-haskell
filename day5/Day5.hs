import Data.List
import Data.Ix

type Coord = ( Int, Int )
type Line = ( Coord, Coord )

type Size = Coord
data Matrix = Matrix { matrixData :: [ Int ]
                     , width :: Int
                     , height :: Int
                     } deriving (Show)

-- parsing
coord :: ( String, String ) -> Coord
coord ( first, second ) = ( read first::Int, read (tail second)::Int )

coordsFromWords :: [String] -> Line
coordsFromWords [ first, delim, second ] = (coord ( break (==',') first ), coord ( break (==',') second ) )

coordsFromRow :: String -> Line
coordsFromRow row = coordsFromWords (words row)

linesFromRows :: [ String ] -> [ Line ]
linesFromRows rows = map coordsFromRow rows

-- problem solving

isStraight :: Line -> Bool
isStraight ((x1,y1), (x2,y2)) = x1 == x2 || y1 == y2

onlyStraight :: [ Line ] -> [ Line ]
onlyStraight ls = filter (isStraight) ls

-- *********
-- When I realised we don't need a matrix for part 1 (but then I put it back for debugging)
maxCoord :: Int -> Int -> [ Line ] -> Coord
maxCoord accX accY [] = ( accX + 1 , accY + 1)
maxCoord accX accY ( ( ( x1, y1 ), ( x2, y2 ) ):ls ) = maxCoord (maximum [ accX, x1, x2 ]) (maximum[accY , y1, y2 ]) ls

initMatrix :: Size -> Matrix
initMatrix (maxX, maxY) = Matrix {matrixData=take (maxX * maxY) [0, 0..], width=maxX, height=maxY}

increaseValue :: Matrix -> Coord -> Matrix
increaseValue m (x, y) = Matrix {
    matrixData=take (width m * y + x) (matrixData m) ++ (matrixData m)!!(width m * y + x) + 1 : drop (width m * y + x + 1) (matrixData m),
    width=width m,
    height=height m
}

increaseValues :: Matrix -> [Coord] -> Matrix
increaseValues m (coord:[]) = increaseValue m coord
increaseValues m (coord:cs) = increaseValues (increaseValues m [coord]) cs

-- shameless stealing
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l
  | n > 0 = (take n l) : (chunk n (drop n l))
  | otherwise = error "Negative or zero n"

-- end of useless stuff
-- *********

interpolateStraightLine :: Line -> [ Coord ]
interpolateStraightLine ((x1, y1), (x2, y2))
    | (x1 == x2) && (y1 == y2) = [(x1, y1)]
    | (x1 < x2) || (y1 < y2) = range ((x1, y1), (x2, y2))
    | otherwise = interpolateStraightLine ((x2, y2), (x1, y1))

interpolateDiagonalLine :: Line -> [ Coord ]
interpolateDiagonalLine ((x1, y1), (x2, y2))
    -- lol it's late
    | (x1 < x2) && (y1 < y2) = zip [x1..x2] [y1..y2]
    | (x1 < x2) && (y1 > y2) = zip [x1..x2] [y1, y1-1..y2]
    | (x1 > x2) && (y1 < y2) = zip [x1, x1-1..x2] [y1..y2]
    | (x1 > x2) && (y1 > y2) = zip [x1, x1-1..x2] [y1, y1-1..y2]

interpolateLine :: Line -> [ Coord ]
interpolateLine l
    | isStraight l = interpolateStraightLine l
    | otherwise = interpolateDiagonalLine l

main = do
    contents <- readFile "day5/input.txt"
    let rows = lines contents
    let ls = linesFromRows rows
    let straightLines = onlyStraight ls
    let coordsToIncrease = concat $ map (interpolateStraightLine) straightLines
    -- we now have all the coordinates a line passes through. So we can just sort and group them, keep the ones
    -- with length >= 2 and check how many of those we have
    putStrLn $ "Answer 1: " ++ show ( length $ filter (\x -> (length x) >= 2) (group ( sort coordsToIncrease ) ) )

    let coordsToIncreaseAll = concat $ map (interpolateLine) ls
    -- back to debugging
    --    let size = (maxCoord 0 0 ls)
    --    print coordsToIncrease
    --    let matrix = increaseValues (initMatrix size)  coordsToIncrease
    --    print matrix
    --    mapM putStrLn ( [ concat [ if x == 0 then "." else (show x) | x <- line ] | line <- chunk (fst size) (matrixData matrix)] )
    --
    --    let matrixAll = increaseValues (initMatrix size)  coordsToIncreaseAll
    --    print matrixAll
    --    mapM putStrLn ( [ concat [ if x == 0 then "." else (show x) | x <- line ] | line <- chunk (fst size) (matrixData matrixAll)] )
    --
    -- end of debugging

    putStrLn $ "Answer 2: " ++ show ( length $ filter (\x -> (length x) >= 2) (group ( sort coordsToIncreaseAll ) ) )
