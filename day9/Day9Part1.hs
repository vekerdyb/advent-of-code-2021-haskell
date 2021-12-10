import Data.List
import Data.Ix

type Coord = ( Int, Int )
type Size = Coord
data Matrix = Matrix { matrixData :: [ Int ]
                     , width :: Int
                     , height :: Int
                     } deriving (Show)

parse :: [String] -> Matrix
parse ls = Matrix {matrixData = concat $ [[read (item:"")::Int | item <- row] | row <- ls], width = length $ ls!!0, height = length ls}

matrixElem :: Coord -> Matrix -> Int
matrixElem (x, y) matrix
    | x < 0 || y < 0 || x > (width matrix - 1) || y > (height matrix - 1) = -1
    | otherwise = (matrixData matrix) !! (y * (width matrix) + x)


neighbours :: Coord -> Matrix -> [Int]
neighbours (x, y) matrix = filter (>=0) [
        matrixElem (x - 1, y) matrix,
        matrixElem (x + 1, y) matrix,
        matrixElem (x, y - 1) matrix,
        matrixElem (x, y + 1) matrix
    ]

lowerThanNeighbours :: Coord -> Matrix -> Bool
lowerThanNeighbours coord matrix = all (\x -> x > matrixElem coord matrix) (neighbours coord matrix)

allCoords :: Matrix -> [Coord]
allCoords matrix = [(x, y) | (y, x) <- range ((0,0), (height matrix - 1, width matrix - 1))]

lowPoints :: Matrix -> [Coord]
lowPoints matrix = filter (\coord -> lowerThanNeighbours coord matrix ) (allCoords matrix)

lowPointValues :: Matrix -> [Int]
lowPointValues matrix = [(matrixElem coord matrix) + 1 | coord <- lowPoints matrix]

main = do
    contents <- readFile "day9/input.txt"
    let matrix = parse $ lines contents
    print $ sum $ lowPointValues matrix
