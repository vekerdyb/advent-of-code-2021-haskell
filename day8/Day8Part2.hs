import Data.List
import Data.Maybe

splitOn ::  Char -> String -> String -> [String]
splitOn separator acc (x:xs)
    | xs == [] = [acc ++ [x]]
    | x == separator = [acc] ++ (splitOn separator [] xs)
    | otherwise = splitOn separator (acc ++ [x]) xs

binary_ :: String -> [Int] -> [Int]
binary_ (x:xs) acc
    | xs == [] = take (ind) acc ++ [1] ++ drop (ind + 1) acc
    | otherwise = binary_ xs (binary_ [x] acc)
    where ind = fromJust $ elemIndex x "abcdefg"

-- turn a string of letters into binary representation
binary :: String -> [Int]
binary str = binary_ str (take 7 $ repeat 0)

-- how many times each segment appears in the unique inputs
binaryCounts :: [String] -> [Int]
binaryCounts uniquePatterns = foldl (\acc x -> zipWith (+) acc x) (take 7 $ repeat 0) (map (binary) uniquePatterns)

{-
how to get to the answer?

Real segment counts in all ten digits
a 8
b 6 - definitive
c 8
d 7
e 4 - definitive
f 9 - definitive
g 7

We know these things:
1) the real segments b, e, f will appear 6, 4, 9 times respectively
2) the real segments a, c will appear 8 times
3) the real segments d, g will appear 7 times.
4) the pattern lengths define
        1 [c, f] = length 2
        4 [b, c, d, f] = length 4
        7 [a, c, f] = length 3
        8 [a, b, c, d, e, f, g] = length 7
5) so the remaining things are:
     - 0 [a, b, c, e, f, g] - length 6 ; doesn't have d = DOES HAVE all letters from 1 (length 2)
     - 6 [a, b, d, e, f, g] - length 6 ; doesn't have c = doesn't have all letters from 1 (length 2) ; not other length 6s
     - 9 [a, b, c, d, f, g] - length 6 ; doesn't have e = doesn't have the letter appearing 4 times
     - 2 [a, c, d, e, g] - length 5 ; doesn't have b, f = doesn't have the letters appearing 6 and 9 times
     - 3 [a, c, d, f, g] - length 5 ; doesn't have b, e = doesn't have the letters appearing 6 and 4 times
     - 5 [a, b, d, f, g] - length 5 ; doesn't have c, e = doesn't have the letter 4 times
-}
letterAppearingNTimes :: [Int] -> Int -> Char
letterAppearingNTimes bCounts n = "abcdefg" !! (fromJust $ elemIndex n bCounts)

containsLetterAppearingNTimes :: [Int] -> String -> Int -> Bool
containsLetterAppearingNTimes bCounts pattern n = (letterAppearingNTimes bCounts n) `elem` pattern

decodePattern :: [Int] ->  String -> String -> (String, Int)
decodePattern bCounts twoLetterPattern pattern
    | length pattern == 2 = (sortedPattern, 1)
    | length pattern == 4 = (sortedPattern, 4)
    | length pattern == 3 = (sortedPattern, 7)
    | length pattern == 7 = (sortedPattern, 8)
    | (length pattern == 6) && (not $ containsLetterAppearingNTimes bCounts pattern 4) = (sortedPattern, 9)
    | (length pattern == 6) && all (\letter -> letter `elem` pattern) twoLetterPattern = (sortedPattern, 0)
    | (length pattern == 6)  = (sortedPattern, 6)
    | not $ any (containsLetterAppearingNTimes bCounts pattern) [6,9] = (sortedPattern, 2)
    | not $ any (containsLetterAppearingNTimes bCounts pattern) [6,4] = (sortedPattern, 3)
    | otherwise = (sortedPattern, 5)
    where sortedPattern = sort pattern

decodeUniquePatterns ::  [Int] -> [String] -> [(String, Int)]
decodeUniquePatterns bCounts patterns = sort $ map (decodePattern bCounts (fromJust $ find (\x -> length x == 2) patterns)) patterns

decodeOutputDigit :: [(String, Int)] -> String -> Int
decodeOutputDigit decodedPatterns outputDigit = snd (fromJust $ find (\(x, _) -> x == (sort outputDigit)) decodedPatterns)

decodeOutput :: [(String, Int)] -> [String] -> Int
decodeOutput decodedPatterns output = foldl (\acc x -> acc * 10 + x) 0 (map (decodeOutputDigit decodedPatterns) output)

main = do
    contents <-  readFile "day8/input.txt"
    let inputs = [splitOn '|' "" line | line <- (lines contents)]
    let (uniquePatterns, outputs) = unzip [(words (input!!0), words (input!!1)) | input <- inputs]
    let parsed = [(words (input!!0), words (input!!1)) | input <- inputs]
    print $ sum [decodeOutput (decodeUniquePatterns ((binaryCounts uniquePatterns)) (uniquePatterns)) output | (uniquePatterns, output) <- parsed]
