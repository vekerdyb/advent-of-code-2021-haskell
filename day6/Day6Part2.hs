sample = [3,4,3,1,2]
input = [4,1,1,1,5,1,3,1,5,3,4,3,3,1,3,3,1,5,3,2,4,4,3,4,1,4,2,2,1,3,5,1,1,3,2,5,1,1,4,2,5,4,3,2,5,3,3,4,5,4,3,5,4,2,5,5,2,2,2,3,5,5,4,2,1,1,5,1,4,3,2,2,1,2,1,5,3,3,3,5,1,5,4,2,2,2,1,4,2,5,2,3,3,2,3,4,4,1,4,4,3,1,1,1,1,1,4,4,5,4,2,5,1,5,4,4,5,2,3,5,4,1,4,5,2,1,1,2,5,4,5,5,1,1,1,1,1,4,5,3,1,3,4,3,3,1,5,4,2,1,4,4,4,1,1,3,1,3,5,3,1,4,5,3,5,1,1,2,2,4,4,1,4,1,3,1,1,3,1,3,3,5,4,2,1,1,2,1,2,3,3,5,4,1,1,2,1,2,5,3,1,5,4,3,1,5,2,3,4,4,3,1,1,1,2,1,1,2,1,5,4,2,2,1,4,3,1,1,1,1,3,1,5,2,4,1,3,2,3,4,3,4,2,1,2,1,2,4,2,1,5,2,2,5,5,1,1,2,3,1,1,1,3,5,1,3,5,1,3,3,2,4,5,5,3,1,4,1,5,2,4,5,5,5,2,4,2,2,5,2,4,1,3,2,1,1,4,4,1,5]

numberOfFishAgedN :: Int -> [Int] -> Int
numberOfFishAgedN age fish = length $ filter (==age) fish

updateCounts :: (Int -> [Int] -> Int) -> [Int] -> [Int]
updateCounts func groupedCounts = [
        if x == 7
        then (func x groupedCounts + func 0 groupedCounts)
        else (func x groupedCounts) | x <- reverse (0 : [8,7..1])
    ]

groupedCountsFromFish :: [Int] -> [Int]
groupedCountsFromFish fish = updateCounts numberOfFishAgedN fish

tickOneDay :: [Int] -> [Int]
tickOneDay groupedCounts = updateCounts (\x ls -> ls!!x) groupedCounts

tickNDay :: Int -> [Int] -> [Int]
tickNDay days groupedCounts = iterate tickOneDay groupedCounts !! (days - 1)

answer :: Int -> [Int] -> Int
answer days fish = sum $ tickNDay days (groupedCountsFromFish fish)

main = do
    print (answer 256 sample)
    print (answer 256 input)
