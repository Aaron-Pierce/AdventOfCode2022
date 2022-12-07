import Data.List (nub, findIndex)

part1 input = do
    let fours = [[input !! i, input !! (i + 1), input !! (i + 2), input !! (i+3)] | i <- [0..(length input - 4)]]
    let dedup = nub <$> fours
    let firstIndex = findIndex (\x -> length x == 4) dedup
    print $ (4+) <$> firstIndex

part2 input = do
    let size = 14
    let groups = [getN i size input | i <- [0..(length input - size)]] where
        getN :: Int -> Int -> [Char] -> [Char]
        getN ind 0 str = ""
        getN ind k str = (str !! ind):(getN (ind+1) (k-1) str) 
    let dedup = nub <$> groups
    let firstIndex = findIndex (\x -> length x == size) dedup
    print $ (size+) <$> firstIndex

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input