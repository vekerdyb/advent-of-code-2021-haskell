import Data.List
import Data.Maybe

openingChars = ['(', '[', '{', '<']
closingChars = [')', ']', '}', '>']

scoreForChar :: Char -> Int
scoreForChar ch
    | ch == ')' = 3
    | ch == ']' = 57
    | ch == '}' = 1197
    | ch == '>' = 25137
    | otherwise = 0

removeFirst :: Char -> String -> String
removeFirst _ ""  = ""
removeFirst ch (a:acceptableNextChars)
    | a == ch = acceptableNextChars
    | otherwise = a : removeFirst ch acceptableNextChars

matching :: Char -> Char
matching ch
    | ch `elem` openingChars = closingChars !! (fromJust $ elemIndex ch openingChars)
    | ch `elem` closingChars = openingChars !! (fromJust $ elemIndex ch closingChars)

parseLine :: String -> String -> Char
parseLine acceptableClosingChars (ch:str)
    | str == [] = if ch `elem` acceptableClosingChars then ' ' else ch
    | ch `elem` openingChars = parseLine ((matching ch):acceptableClosingChars) str
    | ch == acceptableClosingChars!!0 = parseLine (removeFirst ch acceptableClosingChars) str
    | otherwise = ch

main = do
    contents <- readFile "day10/input.txt"
    let ls = lines contents
    print $ foldl (\acc ch -> acc + scoreForChar ch) 0 (map (parseLine "") ls)
