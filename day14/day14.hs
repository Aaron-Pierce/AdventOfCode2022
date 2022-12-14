import Data.Map (Map, mapWithKey, lookup, insert, filter, size, fromList)
import Data.List.Split
import Prelude hiding (lookup)
import Data.Maybe (isNothing)
import Data.List (nub)

type Coordinate = (Int, Int)

cadd :: Coordinate -> Coordinate -> Coordinate
c1 `cadd` c2 = (fst c1 + fst c2, snd c1 + snd c2)

data Tile = Air | Sand | Rock deriving (Show, Eq, Ord)
type Grid = Map Coordinate Tile

parseGrid :: [String] -> Grid 

parseGrid gridlines = do
    let controlPoints = fmap ((\e -> (read <$> e) :: [Int]) . splitOn ",") . splitOn " -> " <$> gridlines
    let gridPoints = controlPoints >>= constructLine
        constructLine line = concat [interp (line !! i) (line !! (i + 1)) | i <- [0..(length line - 2)]]
        interp [p1x, p1y] [p2x, p2y] = do
            xRange <- [(min p1x p2x)..(max p1x p2x)]
            yRange <- [(min p1y p2y)..(max p1y p2y)]
            [(xRange, yRange)] :: [Coordinate]
    let gridKVList = zip (nub gridPoints) (repeat Rock)
    let grid = fromList gridKVList
    grid


type IsVoided = Bool

dropSand :: Grid -> Coordinate -> (Coordinate -> Grid -> Maybe Tile) -> (Coordinate -> Bool) -> (Coordinate -> Bool) -> (Grid, IsVoided)
dropSand grid sourcePoint fetchFn earlyFn stoppedFn
    | earlyFn sourcePoint = (grid, True) -- For part1, an early stopping condition (sand is in the void, below any rocks)
    | empty tileBelow = dropSand grid (sourcePoint `cadd` (0, 1)) fetchFn earlyFn stoppedFn
    | empty tileLeftBelow = dropSand grid (sourcePoint `cadd` (-1, 1)) fetchFn earlyFn stoppedFn
    | empty tileRightBelow = dropSand grid (sourcePoint `cadd` (1, 1)) fetchFn earlyFn stoppedFn
    | otherwise = (Data.Map.insert sourcePoint Sand grid, stoppedFn sourcePoint) -- stoppedFn is for part2, if we place sand at (500, 0) then stop, part1 uses const False
    where 
          tileBelow = fetchFn (sourcePoint `cadd` (0, 1)) grid -- Fetch fn allows part2 to generate the ground by inspecting the coord, part1 just uses Data.Map.lookup
          tileLeftBelow = fetchFn (sourcePoint `cadd` (-1, 1)) grid
          tileRightBelow = fetchFn (sourcePoint `cadd` (1, 1)) grid
          empty t = isNothing t || t == Just Air
          full t = t == Just Rock || t == Just Sand


part1_inner grid voidLevel numPlaced = do
    let result = dropSand grid (500, 0) lookup (\c -> snd c > voidLevel) (const False)
    if snd result then result else part1_inner (fst result) voidLevel (numPlaced + 1)


part1 input = do
    let grid = parseGrid (lines input)
    let maxYValue = maximum $ mapWithKey (\key _ -> snd key) grid   
    let final = part1_inner grid maxYValue 0
    print $ length $ Data.Map.filter (== Sand) (fst final)


part2_inner grid groundLevel = do
    let result = dropSand grid (500, 0) (\coord g -> (if snd coord >= groundLevel then Just Rock else coord `lookup` g)) (const False) (\c -> c == (500, 0))
    if snd result then result else part2_inner (fst result) groundLevel

part2 input = do
    let grid = parseGrid (lines input)
    let maxYValue = maximum $ mapWithKey (\key _ -> snd key) grid   
    let final = part2_inner grid (maxYValue+2)
    print $ length $ Data.Map.filter (== Sand) (fst final)

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input