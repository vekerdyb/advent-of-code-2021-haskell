import Data.List
import Data.Ix
size = 10

indexFromCoords :: (Int, Int) -> Int
indexFromCoords (x, y) = (y * size + x)

exists :: (Int, Int) -> Bool
exists (x, y) = x < size && x > -1 && y < size && y > -1

octopus :: [Int] -> (Int, Int) -> Int
octopus octopuses (x, y)
    | exists (x, y) = octopuses!!(indexFromCoords (x, y))
    | otherwise = -1

set :: [Int] -> (Int, Int) -> Int -> [Int]
set octopuses coords value  = (take $ indexFromCoords coords) octopuses
                               ++ value:(drop (indexFromCoords coords + 1) octopuses)

neighbours :: [Int] -> (Int, Int) -> [Int]
neighbours octopuses (x, y) = map (octopus octopuses) (neighbourCoords (x, y))

neighbourCoords :: (Int, Int) -> [(Int, Int)]
neighbourCoords (x, y) = filter (\(x_, y_) -> exists (x_, y_)) [
        (x', y') |
        (x', y') <- range ((x - 1, y - 1), (x + 1, y + 1))
        , not (x' == x && y' == y)
    ]

allCoords :: [(Int, Int)]
allCoords = [(x, y) | (y, x) <- range((0,0), (size - 1,size - 1))]

increaseBy1 ::  [Int] -> (Int, Int)  -> [Int]
increaseBy1 octopuses coords  = (take $ indexFromCoords coords) octopuses
                               ++ (octopus octopuses coords + 1):(drop (indexFromCoords coords + 1) octopuses)

increaseAllBy1 :: [Int] -> [Int]
increaseAllBy1 octopuses = map (+1) octopuses

--maybeFlash :: [Int] -> [(Int, Int)] -> (Int, Int) -> [Int]
--maybeFlash octopuses alreadyFlashed coords
--    | coords `elem` alreadyFlashed = octopuses
--    | octopus octopuses coords > 9 = neighbourCoords octopuses coords

flashers :: [Int] -> [(Int, Int)]
flashers octopuses = [coords | coords <- allCoords, octopus octopuses coords > 9 ]

increaseNeighbours :: [Int] -> (Int, Int) -> [Int]
increaseNeighbours octopuses coords = [
        if (x, y) `elem` neighbourCoords coords
            then (octopus octopuses (x, y) + 1)
            else (octopus octopuses (x, y)) |
            (x, y) <- allCoords
    ]

flashingStep :: [Int] -> [(Int, Int)] -> [(Int, Int)] -> [Int]
flashingStep octopuses alreadyFlashed willFlash
    | willFlash == [] = map (\x -> if x > 9 then 0 else x) octopuses
    | otherwise = flashingStep (increasedNeighbours) (firstCoord:alreadyFlashed) newWillFlash
    where firstCoord:coords = willFlash
          increasedNeighbours = increaseNeighbours octopuses firstCoord
          newWillFlash = [
                    c | c <- flashers increasedNeighbours
                    , c `notElem` willFlash
                    , c `notElem` alreadyFlashed
                ] ++ coords

stepN :: [Int] -> Int -> [Int]
stepN octopuses 0 = octopuses
stepN octopuses n = stepN (flashingStep (increaseAllBy1 octopuses) [] (flashers (increaseAllBy1 octopuses))) (n - 1)

countFlashesInNSteps :: [Int] -> Int -> Int -> Int
countFlashesInNSteps octopuses flashesSoFar n
    | n == 0 = flashesSoFar
    | otherwise = countFlashesInNSteps steppedOctopuses (flashesSoFar + (length $ filter (==0) steppedOctopuses)) (n - 1)
    where steppedOctopuses = flashingStep (increaseAllBy1 octopuses) [] (flashers $ increaseAllBy1 octopuses)

findFirstSync :: [Int] -> Int -> Int
findFirstSync octopuses steps
    | sum octopuses == 0 = steps
    | otherwise = findFirstSync (stepN octopuses 1) steps + 1

-- helpers for debugging

chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk n l
  | n > 0 = (take n l) : (chunk n (drop n l))
  | otherwise = error "Negative or zero n"

printOctopuses :: [Int] -> IO ()
printOctopuses octopuses = mapM_ putStrLn [ concat $ (intersperse " " [show o | o <- row ]) | row <- chunk size octopuses]

main = do
    contents <- readFile "day11/input.txt"
    let octopuses = concat $ [[read (item:"")::Int | item <- row] | row <- lines contents]
    putStrLn $ "Part 1: " ++ show (countFlashesInNSteps octopuses 0 100)
    putStrLn $ "Part 2: " ++ show (findFirstSync octopuses 0)