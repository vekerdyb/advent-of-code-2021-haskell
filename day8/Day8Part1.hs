splitOn ::  Char -> String -> String -> [String]
splitOn separator accumulator (x:xs)
    | xs == [] = [accumulator ++ [x]]
    | x == separator = [accumulator] ++ (splitOn separator [] xs)
    | otherwise = splitOn separator (accumulator ++ [x]) xs

main = do
    contents <-  readFile "day8/input.txt"
    let inputs = [splitOn '|' "" line | line <- (lines contents)]
    let (uniquePatterns, outputs) = unzip [(words (input!!0), words (input!!1)) | input <- inputs]
    print $ length $ concat ([filter (\x -> (length x) `notElem` [5, 6]) output | output <- outputs])