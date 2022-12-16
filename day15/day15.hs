import Text.Parsec
import Text.Parsec.Char 
import Data.List (nub)


type Coordinate = (Int, Int)


intParser = do
    sign <- option 1 (do{
        char '-';
        return (-1);
    })
    digits <- many digit
    return $ (read digits :: Int) * sign

sensorReadingParser = do
    string "Sensor at x="
    xPos <- intParser
    string ", y="
    yPos <- intParser
    string ": closest beacon is at x="
    beaconX <- intParser
    string ", y="
    beaconY <- intParser
    return ((xPos, yPos), (beaconX, beaconY))


parseSensorReading :: String -> (Coordinate, Coordinate)
parseSensorReading line = do
    let result = runParser sensorReadingParser () "SensorReadingParser" line
    case result of
        Left err -> error $ show err
        Right pair -> pair 


manhattanDist :: Coordinate -> Coordinate -> Int
manhattanDist (c1x, c1y) (c2x, c2y) = abs(c1x - c2x) + abs(c1y - c2y)

isFartherThanReading :: Coordinate -> (Coordinate, Coordinate) -> Bool
isFartherThanReading location (sensorPos, nearestBeacon) = manhattanDist sensorPos nearestBeacon < manhattanDist sensorPos location

isPlaceable :: [(Coordinate, Coordinate)] -> Coordinate -> Bool
isPlaceable sensorReadings potentialLocation = and $ isFartherThanReading potentialLocation <$> sensorReadings

allAtDistance :: Coordinate -> Int -> [Coordinate]
allAtDistance currentPoint dist = do
    deltas <- [0..dist]
    topRightPoints <- [(fst currentPoint + deltas, snd currentPoint - (dist - deltas))]
    bottomRightPoints <- [(fst currentPoint + deltas, snd currentPoint + (dist - deltas))]
    bottomLeftPoints <- [(fst currentPoint - deltas, snd currentPoint + (dist - deltas))]
    topLeftPoints <- [(fst currentPoint - deltas, snd currentPoint - (dist - deltas))]
    [topRightPoints, bottomRightPoints, bottomLeftPoints, topLeftPoints]


inBoundingBox :: (Coordinate, Coordinate) -> Coordinate -> Bool
inBoundingBox (bboxL, bboxR) point = (fst bboxL <= fst point && fst point <= fst bboxR) && (snd bboxL <= snd point && snd point <= snd bboxR)

part1 input = do
    let inputlines = lines input
    let sensorReadings = parseSensorReading <$> inputlines
    let allXs = (fst . fst <$> sensorReadings) ++ (fst . snd <$> sensorReadings)
    let allDistances = uncurry manhattanDist <$> sensorReadings
    let potentialLocations = zip [(minimum allXs - maximum allDistances)..(maximum allXs + maximum allDistances)] (repeat 2000000)
    let valids = (\c -> isPlaceable sensorReadings c || (c `elem` (snd <$> sensorReadings))) <$> potentialLocations
    print $ length $ filter not valids

part2 input = do
    let inputlines = lines input
    let sensorReadings = parseSensorReading <$> inputlines
    let fringes = (\(sensor, beacon) -> allAtDistance sensor (manhattanDist beacon sensor + 1)) <$> sensorReadings
    let searchSpace = filter (inBoundingBox ((0, 0), (4000000,4000000))) (concat fringes)

    let valids = filter (\c -> isPlaceable sensorReadings c && (c `notElem` (snd <$> sensorReadings))) searchSpace
    let uniqueSolutions = nub valids
    if length uniqueSolutions == 1
        then let (solX, solY) = head uniqueSolutions in print (solX * 4000000 + solY)
        else error $ "More than one unique solution: " ++ (show uniqueSolutions)    
    


main = do
    input <- readFile "input.txt"
    part1 input
    part2 input
