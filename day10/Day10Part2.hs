import Data.List
import Data.Maybe

openingChars = ['(', '[', '{', '<']
closingChars = [')', ']', '}', '>']

scoreForChar :: Char -> Int
scoreForChar ch
    | ch == ')' = 1
    | ch == ']' = 2
    | ch == '}' = 3
    | ch == '>' = 4
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

parseLine :: String -> String -> String
parseLine acceptableClosingChars (ch:[])
    | ch `elem` openingChars = (matching ch):acceptableClosingChars
    | ch == acceptableClosingChars!!0 = (removeFirst ch acceptableClosingChars)
    | otherwise =  ""
parseLine acceptableClosingChars (ch:str)
    | ch `elem` openingChars = parseLine ((matching ch):acceptableClosingChars) str
    | ch == acceptableClosingChars!!0 = parseLine (removeFirst ch acceptableClosingChars) str
    | otherwise =  ""

score :: Int -> String -> Int
score total (ch:[]) = total * 5 + scoreForChar ch
score total (ch:str) =  score (total * 5 + scoreForChar ch) str

main = do
    contents <- readFile "day10/input.txt"
    let incomplete = filter (/="") $ map (parseLine "") (lines contents)
    print $ (sort $ map (score 0) incomplete) !! (length incomplete `div` 2)
