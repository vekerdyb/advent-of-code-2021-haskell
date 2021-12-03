import Data.List

getMostCommonDigit :: [Char] -> Char
getMostCommonDigit column
    | zeros > ones = '0'
    | otherwise = '1'
    where (zeros, ones) = foldl (\(zs, os) x -> if x == '0' then (zs + 1, os) else (zs, os + 1)) (0,0) column

getColumnCounts :: [String] -> String
getColumnCounts readings = [getMostCommonDigit column | column <- transpose readings]

getOxygenGeneratorRating :: String -> Int -> [String] -> String
getOxygenGeneratorRating columnCounts currentColumn readings
    | length readings == 1 = head readings
    | otherwise = getOxygenGeneratorRating (getColumnCounts remainingReadings) (currentColumn + 1) (remainingReadings)
    where remainingReadings = filter (\x -> x!!currentColumn == columnCounts!!currentColumn) readings

getCO2ScrubberRating :: String -> Int -> [String] -> String
getCO2ScrubberRating columnCounts currentColumn readings
    | length readings == 1 = head readings
    | otherwise = getCO2ScrubberRating (getColumnCounts remainingReadings) (currentColumn + 1) (remainingReadings)
    where remainingReadings = filter (\x -> x!!currentColumn /= columnCounts!!currentColumn) readings

bin2int :: String -> Int
bin2int [] = 0
bin2int (x:xs) = (read [x]::Int) * 2 ^ (length xs) + (bin2int xs)

-- problem 2 -- I've done problem 1 in Google Sheet, probably adding haskell later :D
main = do
    contents <- readFile "day3/input.txt"
    let readings = lines contents
    let columnCounts = [getMostCommonDigit column | column <- transpose readings]
    let oxygen = getOxygenGeneratorRating columnCounts 0 readings
    let co2 = getCO2ScrubberRating columnCounts 0 readings
    putStrLn $ "Sample"
    putStrLn $ "Column counts: " ++ show ( getColumnCounts readings )
    putStrLn $ "Problem 2 Oxygen: " ++ show oxygen
    putStrLn $ "Problem 2 CO2: " ++ show co2
    putStrLn $ "Answer part 2: " ++ show (bin2int oxygen * bin2int co2)