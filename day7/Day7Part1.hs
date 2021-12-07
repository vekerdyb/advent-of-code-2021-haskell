import Data.List

sample = [16,1,2,0,4,2,7,1,2,14]

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

main = do
    contents <- readFile "day7/input.txt"
    let locations = map (\x -> read x::Int) (wordsWhen (==',') contents)
    let median = ((sort locations) !! (length locations `div` 2))
    print $ foldl (\acc x -> acc + abs (x - median)) 0 locations
