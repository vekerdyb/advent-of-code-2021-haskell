-- not working

import Data.List
import Data.Ix

type Coord = ( Int, Int )
type Size = Coord
data Matrix = Matrix { matrixData :: [ (Coord, Int) ]
                     , width :: Int
                     , height :: Int
                     } deriving (Show)

parse :: [String] -> Matrix
parse ls = Matrix {
        matrixData = zip (allCoords (length $ ls!!0) (length ls))  (concat $ [[read (item:"")::Int | item <- row] | row <- ls]),
        width = length $ ls!!0,
        height = length ls
    }

allCoords :: Int -> Int -> [Coord]
allCoords width height = [(x, y) | (y, x) <- range ((0,0), (height - 1, width - 1))]

matrixValue :: Coord -> Matrix -> (Coord, Int)
matrixValue (x, y) matrix
    | x < 0 || y < 0 || x >= (width matrix) || y > (height matrix) = ((x, y), -1)
    | otherwise = (matrixData matrix) !! (y * (width matrix) + x)

neighbours :: Coord -> Matrix -> [(Coord, Int)]
neighbours (x, y) matrix = filter (\(_, v) -> v >= 0) [
        matrixValue (x - 1, y) matrix,
        matrixValue (x + 1, y) matrix,
        matrixValue (x, y - 1) matrix,
        matrixValue (x, y + 1) matrix
    ]

lowerThanNeighbours :: Coord -> Matrix -> Bool
lowerThanNeighbours coord matrix = all (\(c, v) -> v > snd (matrixValue coord matrix)) (neighbours coord matrix)

lowPoints :: Matrix -> [Coord]
lowPoints matrix = filter (\coord -> lowerThanNeighbours coord matrix ) [coord | (coord, _) <- matrixData matrix]


-- basin lowPoint
-- - take all neighbours
-- - if not 9 and not in the list, add to the list and call basin for this element
-- - if 9 add nothing

basinFrom :: Coord -> Matrix -> [Coord] -> [Coord]
basinFrom coords matrix acc
    | value == 9 = acc
    | length newCoords > 0 = basinFrom (head newCoords) matrix (coords:acc ++ tail newCoords)
    | otherwise = coords:acc
   where ((x, y), value) = matrixValue coords matrix
         newCoords = [c | (c, v) <- neighbours coords matrix, c `notElem` acc, v /= 9]

main = do
    contents <- readFile "day9/sample.txt"
    let matrix = parse $ lines contents
    print $ matrix
    print $ lowPoints matrix

--    print $ [basinFrom coord matrix [] | coord <- lowPoints matrix]
--    putStrLn $ "[]" ++ (show $ neighbours (0, 8) matrix)
--    putStrLn $ "[((8, 0), 1), ((9, 1), 1)]" ++ (show $ neighbours (9, 0) matrix)
--    print $ [neighbours n matrix |  (n, _) <- neighbours (9, 0) matrix]