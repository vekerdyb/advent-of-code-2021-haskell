import Data.List

sample = [16,1,2,0,4,2,7,1,2,14]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

triangularNumber :: Int -> Int
triangularNumber x = x * (x + 1) `div` 2

main = do
    contents <- readFile "day7/input.txt"
    let locations = map (\x -> read x::Int) (wordsWhen (==',') contents)
    print $ minimum [sum [triangularNumber $ abs (location - target) | location <-  locations] | target <- [0..maximum locations]]
