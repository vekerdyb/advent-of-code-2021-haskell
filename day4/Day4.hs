import Data.List
sampleDraw = [7,4,9,5,11,17,23,2,0,14,21,24,10,16,13,6,15,25,12,22,18,20,8,19,3,26,1]
realDraw = [50,68,2,1,69,32,87,10,31,21,78,23,62,98,16,99,65,35,27,96,66,26,74,72,45,52,81,60,38,57,54,19,18,77,71,29,51,41,22,6,58,5,42,92,85,64,94,12,83,11,17,14,37,36,59,33,0,93,34,70,97,7,76,20,3,88,43,47,8,79,80,63,9,25,56,75,15,4,82,67,39,30,89,86,46,90,48,73,91,55,95,28,49,61,44,84,40,53,13,24]

splitOn ::  String -> [String] -> [String] -> [[String]]
splitOn separator accumulator (x:xs)
    | xs == [] = [accumulator ++ [x]]
    | x == separator = [accumulator] ++ (splitOn separator [] xs)
    | otherwise = splitOn separator (accumulator ++ [x]) xs

getBoards :: [String] -> [[[Int]]]
getBoards ls = [ [ [ read num::Int | num <- words boardLine] | boardLine <- board]  | board <- splitOn "" [] ls]

isWinningLine :: [Int] -> [Int] -> Bool
isWinningLine drawnNumbers line = all (==True) (map (\num -> num `elem` drawnNumbers) line)

isBoardLineWinner :: [Int] -> [[Int]] -> Bool
isBoardLineWinner drawnNumbers board = any (==True) (map (\line -> isWinningLine drawnNumbers line) board)

isBoardWinner :: [Int] -> [[Int]] -> Bool
isBoardWinner drawnNumbers board = isBoardLineWinner drawnNumbers board || isBoardLineWinner drawnNumbers (transpose board)

findFirstWinnerBoard :: [Int] -> [Int] -> [[[Int]]] -> ([[Int]], [Int])
findFirstWinnerBoard alreadyDrawn drawFrom boards
    | length winners > 0 = (winners !! 0, alreadyDrawn)
    | drawFrom == [] = ([], alreadyDrawn)
    | otherwise = findFirstWinnerBoard (alreadyDrawn ++ [head drawFrom]) (tail drawFrom) boards
    where winners = filter (isBoardWinner alreadyDrawn) boards

findAllWinnerBoards :: [Int] -> [Int] -> [[[Int]]] -> [([[Int]], [Int])]
findAllWinnerBoards alreadyDrawn drawFrom boards
    | drawFrom == [] = []
    | boards == [] = []
    | otherwise = findAllWinnerBoards alreadyDrawn drawFrom (tail boards) ++
        [findFirstWinnerBoard alreadyDrawn drawFrom [head boards]]

findLastWinnerBoard :: [([[Int]], [Int])] -> ([[Int]], [Int])
findLastWinnerBoard winnerBoards =  last $ sortBy (\a b -> if length (snd a) < length (snd b) then LT else GT) winnerBoards

sumUnmarked :: [Int] -> [[Int]] -> Int
sumUnmarked drawnNumbers board = sum $ filter (`notElem` drawnNumbers) (concat board)

main = do
    contents <- readFile "day4/input.txt"
    let draw = realDraw
    let boards = getBoards ( lines contents )
    putStrLn $ "Sample"
    putStrLn $ show (lines contents)
    putStrLn $ "Boards: " ++ show (boards)
    let (winnerBoard, drawnNumbers) = findFirstWinnerBoard [] draw boards
    putStrLn $ "Winner: " ++ show (winnerBoard)
    putStrLn $ "Drawn numbers: " ++ show (drawnNumbers)
    let unmarked = sumUnmarked drawnNumbers winnerBoard
    putStrLn $ "Sum unmarked: " ++ show (unmarked)
    let lastDrawn = last drawnNumbers
    putStrLn $ "Last drawn: " ++ show (lastDrawn)
    putStrLn $ "Answer: " ++ show (unmarked * lastDrawn)
    -- part 2
    let allWinnerBoards = findAllWinnerBoards [] draw boards
    putStrLn $ "All winners: " ++ show (allWinnerBoards)
    let (lastWinnerBoard, lastDrawnNumbers) = findLastWinnerBoard allWinnerBoards
    putStrLn $ "Last winner: " ++ show (lastWinnerBoard)
    let lastUnmarked = sumUnmarked lastDrawnNumbers lastWinnerBoard
    putStrLn $ "Sum unmarked: " ++ show (lastUnmarked)
    let lastLastDrawn = last lastDrawnNumbers
    putStrLn $ "Answer: " ++ show (lastUnmarked * lastLastDrawn)
