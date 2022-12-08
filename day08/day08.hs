import Data.List.Split (splitOn)
import Data.List (transpose)



-- This is so gross.
-- Im sure there's some way to represent this really nicely with non-determinism


parseInput :: String -> [[Int]]
parseInput input = fmap read . filter (not . null) . splitOn "" <$> lines input

partitionOn :: [a] -> Int -> ([a], a, [a])
partitionOn list index = let (left, right) = splitAt index list in (left, head right, tail right)

type TreeAndRow = ([Int], Int, [Int])

listOnEitherSide :: [[Int]] -> [[TreeAndRow]]
listOnEitherSide matrix = [[(matrix !! row) `partitionOn` col | col <- [0..ncols-1]] | row <- [0..nrows-1]]
    where (nrows, ncols) = (length matrix, length $ head matrix)

determineVisibility :: [[TreeAndRow]] -> [[(Bool, Bool)]]
determineVisibility matrix = fmap (\(leftList, el, rightList) -> (all (<el) leftList, all (<el) rightList)) <$> matrix


determineView :: [[TreeAndRow]] -> [[(Int, Int)]]
determineView matrix = fmap (\(leftList, el, rightList) -> ((length $ combineOne $ span (<el) (reverse leftList), length $ combineOne $ span (<el) (rightList)))) <$> matrix

combineOne :: ([a], [a]) -> [a]
combineOne (left, []) = left
combineOne ([], right) = [head right]
combineOne (left, right) = left ++ [head right] 


part1 input = do
    let matrix = parseInput input
    let (nrows, ncols) = (length matrix, length $ head matrix)

    let broken = listOnEitherSide matrix
    let brokenTransposed = listOnEitherSide (transpose matrix)
    let visibles = determineVisibility broken
    let visiblesTransposed = determineVisibility brokenTransposed
    let combined = fmap (uncurry (||)) <$> visibles
    let combinedTransposed = fmap (uncurry (||)) <$> visiblesTransposed
    let combinedUntranposed = transpose combinedTransposed

    let joined = [[(combined !! row !! col) || (combinedUntranposed !! row !! col) | col <- [0..ncols-1]] | row <- [0..nrows-1]]

    print $ sum (length . filter id <$> joined)


part2 input = do
    let matrix = parseInput input
    let (nrows, ncols) = (length matrix, length $ head matrix)

    let broken = listOnEitherSide matrix
    let brokenTransposed = listOnEitherSide (transpose matrix)
    let views = determineView broken
    let viewsUntransposed = transpose $ determineView brokenTransposed

    let viewProducts = [[(let tup = (views !! row !! col) in (fst tup * snd tup)) * (let tup = (viewsUntransposed !! row !! col) in (fst tup * snd tup)) | col <- [0..ncols-1]] | row <- [0..nrows-1]]

    let maxes = maximum $ maximum <$> viewProducts
    print maxes
    
    
    
main = do
    input <- readFile "input.txt"
    part1 input
    part2 input