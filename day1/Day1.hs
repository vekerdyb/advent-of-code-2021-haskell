import System.IO

testData = [199, 200, 208, 210, 200, 207, 240, 269, 260, 263]

numIncreased :: [Integer] -> Integer
numIncreased xs = sum [1 | (x, y) <- zip xs (tail xs), y > x]

slidingWindowSums :: [Integer] -> [Integer]
slidingWindowSums (x:y:z:[]) = [sum [x,y,z]]
slidingWindowSums (x:y:z:xs) = sum [x,y,z] : slidingWindowSums (y:z:xs)

main = do
    contents <- readFile "day1/input.txt"
    let xs = map (\x -> read x::Integer) (lines contents)
    putStrLn $ "Problem 1: " ++ show (numIncreased xs)
    putStrLn $ "Problem 2: " ++ show (numIncreased $ slidingWindowSums xs)