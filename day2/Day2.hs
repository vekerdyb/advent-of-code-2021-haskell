import System.IO

getNewPos1 :: (Int, Int) -> (String, Int) -> (Int, Int)
getNewPos1 position commandAmount
  |  command == "forward" = (fst position + x, snd position)
  |  command == "down" = (fst position, snd position + x)
  |  command == "up" = (fst position, snd position - x)
  where (command, x) = commandAmount

getNewPos2 :: (Int, Int, Int) -> (String, Int) -> (Int, Int, Int)
getNewPos2 position commandAmount
  |  command == "forward" = (horizontal + x, depth + (aim * x), aim)
  |  command == "down" = (horizontal, depth, aim + x)
  |  command == "up" = (horizontal, depth, aim - x)
  where (command, x) = commandAmount
        (horizontal, depth, aim) = position

data Position = PositionOnly (Int, Int) | PositionAndAim (Int, Int, Int)

getFinalAnswer :: Position -> Int
getFinalAnswer (PositionOnly (x, y)) = x * y
getFinalAnswer (PositionAndAim (x, y, _)) = x * y

main = do
    contents <- readFile "day2/input.txt"
    let xs = [(x, read y::Int) | [x, y] <- map words (lines contents)]
    let finalPos1 = foldl (getNewPos1) (0, 0) xs
    let finalPos2 = foldl (getNewPos2) (0, 0, 0) xs
    putStrLn $ "Problem 1: " ++ show (getFinalAnswer $ PositionOnly finalPos1)
    putStrLn $ "Problem 2: " ++ show (getFinalAnswer $ PositionAndAim finalPos2)