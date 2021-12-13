import Data.List
import Data.Ix

splitOn :: Char -> String -> (String, String)
splitOn separator str = (takeWhile (/= separator) str, tail $ dropWhile (/= separator) str)

unique :: Eq a => [a] -> [a]
unique [] = []
unique (x:xs) = x:unique (filter ((/=) x) xs)

foldUpCoords :: Int -> (Int, Int) -> (Int, Int)
foldUpCoords foldLine (x, y)
    | foldLine >= y = (x, y)
    | otherwise = (x, foldLine - (y - foldLine))

foldLeftCoords :: Int -> (Int, Int) -> (Int, Int)
foldLeftCoords foldLine (x, y)
    | foldLine >= x = (x, y)
    | otherwise = (foldLine - (x - foldLine), y)

foldCoords :: Char -> Int -> (Int, Int) -> (Int, Int)
foldCoords axis
    | axis == 'x' = foldLeftCoords
    | otherwise = foldUpCoords

foldOne :: (Char, Int) -> [(Int, Int)] -> [(Int, Int)]
foldOne (axis, foldLine) listCoords = unique $ filter (\(x, y) -> x >= 0 && y >= 0)
                                          [ foldCoords axis foldLine coords | coords <- listCoords ]

foldAll :: [(Char, Int)] -> [(Int, Int)] -> [(Int, Int)]
foldAll [] listCoords = listCoords
foldAll (f:folds) listCoords = foldAll folds (foldOne f listCoords)

-- utils for pretty print
maxCoords_ :: Int -> Int -> [(Int, Int)] -> (Int, Int)
maxCoords_ maxX maxY [] = ( maxX , maxY )
maxCoords_ maxX maxY ((x, y):coords)= maxCoords_ (maximum [ maxX, x ]) (maximum [maxY , y ]) coords

maxCoords :: [(Int, Int)] -> (Int, Int)
maxCoords coords = maxCoords_ 0 0 coords

allCoords :: [(Int, Int)] -> [(Int, Int)]
allCoords coords = [(y, x) | (x, y) <- range ((0,0), (snd $ maxCoords coords, fst $ maxCoords coords))]

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l
  | n > 0 = (take n l) : (chunk n (drop n l))
  | otherwise = error "Negative or zero n"

printMatrix :: [(Int, Int)] -> IO ()
printMatrix coords = mapM_ putStrLn $ chunk ((fst $ maxCoords coords) + 1)
                           [if c `elem` coords then '#' else '.' | c <- allCoords coords]

main = do
    ls <- readFile "day13/input.txt"
    let coords = map (\c -> (read $ (fst c)::Int, read $ (snd c)::Int))
                    . map (splitOn ',')
                    . takeWhile (/="") $ lines ls
    let folds = map (\f -> (head $ fst f, read $ (snd f)::Int))
                    . map (splitOn '=' . drop 11)
                    . drop (length coords + 1) $ lines ls
    putStrLn $ "Part 1: " ++ show (length $ foldOne (head $ folds) coords)
    putStrLn $ "Part 2:"
    printMatrix $ foldAll folds coords
