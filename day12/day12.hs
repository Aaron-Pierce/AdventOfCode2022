import Data.Map
import Data.Maybe (catMaybes, mapMaybe, fromJust, isJust)
import Data.List (sortBy, minimumBy)
import Data.Set


-- This is so gross but it works

type Elevation = (Char, Int)
type Coordinate = (Int, Int) 

cadd :: Coordinate -> Coordinate -> Coordinate
c1 `cadd` c2 = (fst c1 + fst c2, snd c1 + snd c2)

join :: a -> b -> (a, b)
join a b = (a, b)


convertEndpointElevs :: Char -> Char
convertEndpointElevs c = case c of
    'S' -> 'a'
    'E' -> 'z'
    _ -> c

makeMap :: [String] -> (Map Coordinate Elevation, (Coordinate, Coordinate))
makeMap rows = do
    let elevations = do
            r <- rows
            c <- r
            return (c, 2^31)
    let coordinates = do
            r <- [0..(length rows - 1)]
            c <- [0..(length (head rows) - 1)]
            return (r, c)
    let pre_m = Data.Map.fromAscList (zip coordinates elevations)
    let startAndEnd = Data.Map.toList $ Data.Map.filter (\elev -> fst elev == 'S' || fst elev == 'E') pre_m
    let sortedStartAndEnd = sortBy (\(coord1, elev1) (coord2, elev2) -> elev1 `compare` elev2) startAndEnd
    let m_converted_endpoints = Data.Map.map (\elev -> (convertEndpointElevs $ fst elev, if fst elev == 'S' then 0 else snd elev)) pre_m
    (m_converted_endpoints, (fst $ sortedStartAndEnd!!1, fst $ sortedStartAndEnd!!0))
    

getReachableNeighbors :: Coordinate -> Map Coordinate Elevation -> Char -> [(Coordinate, Elevation)]
getReachableNeighbors coord map heightChar = do
    let maybeNeighbors = (\c -> (c, c`Data.Map.lookup` map)) <$> (getAdjacentCoordinates coord)
    let filteredNeighbors = Prelude.filter (\(c, me) -> isJust me) maybeNeighbors
    let unwrappedNeighbors = (\(c, me) -> (c, fromJust me)) <$> filteredNeighbors 
    let reachableNeighbors = Prelude.filter (\(c, (elevHeight, _)) -> elevHeight <= succ heightChar) unwrappedNeighbors
    reachableNeighbors

getAdjacentCoordinates :: Coordinate -> [Coordinate]
getAdjacentCoordinates coord = [coord `cadd` (-1, 0),
                                coord `cadd` (1, 0),
                                coord `cadd` (0, -1),
                                coord `cadd` (0, 1)]



dijkstrasFrontier :: (Data.Set.Set Coordinate, Map Coordinate Elevation, Data.Set.Set Coordinate) -> (Data.Set.Set Coordinate, Map Coordinate Elevation, Data.Set.Set Coordinate)

dijkstrasFrontier (unvisited, map, frontier)
    | Data.Set.null unvisited = (unvisited, map, frontier)
    | Data.Set.null frontier = (unvisited, map, frontier)
    | otherwise = do
        let lookups = (\c -> (c, c `Data.Map.lookup` map)) <$> (Data.Set.toList frontier)
        let unvisitedElevations = (\(c, me) -> (c, fromJust me)) <$> (Prelude.filter (isJust . snd) $ lookups)
        let unvisitedSorted = sortBy (\(c1, e1) (c2, e2) -> snd e1 `compare` snd e2) unvisitedElevations
        let toVisit = head unvisitedSorted
        let (coordToVisit, elevToVisit) = toVisit



        let neighbors = getReachableNeighbors coordToVisit map (fst elevToVisit)
        let unvisitedNeighbors = Prelude.filter (\e -> fst e `Data.Set.member` unvisited) neighbors
        let updatedNeighbors = (\(coord, elev) -> (coord, (fst elev, min (snd elev) (snd elevToVisit + 1)))) <$> unvisitedNeighbors :: [(Coordinate, Elevation)]
        let updatedNeighborsAsMap = Data.Map.fromAscList updatedNeighbors
        
        let newMap = Prelude.foldl (\m pair -> Data.Map.update (\_ -> Just (snd pair)) (fst pair) m) map updatedNeighbors
        let newUnvisitedSet = coordToVisit `Data.Set.delete` unvisited
        let newFrontier = Prelude.foldl (\s el -> el `Data.Set.insert` s) (coordToVisit `Data.Set.delete` frontier) (fst <$> unvisitedNeighbors) 
        

        dijkstrasFrontier (newUnvisitedSet, newMap, newFrontier)




dijkstras :: (Data.Set.Set Coordinate, Map Coordinate Elevation) -> (Data.Set.Set Coordinate, Map Coordinate Elevation)

dijkstras (unvisited, map)
    | Data.Set.null unvisited = (unvisited, map)
    | otherwise = do
        -- let lookups = (\c -> (c, c `Data.Map.lookup` map)) <$> (Data.Set.toList unvisited)
        -- let unvisitedElevations = (\(c, me) -> (c, fromJust me)) <$> (Prelude.filter (isJust . snd) $ lookups)
        let unvisitedElevations = Prelude.filter (\(k, v) -> k `Data.Set.member` unvisited) (Data.Map.toList map)
        let unvisitedSorted = sortBy (\(c1, e1) (c2, e2) -> snd e1 `compare` snd e2) unvisitedElevations
        let toVisit = head unvisitedSorted
        let (coordToVisit, elevToVisit) = toVisit



        let neighbors = getReachableNeighbors coordToVisit map (fst elevToVisit)
        let unvisitedNeighbors = Prelude.filter (\e -> fst e `Data.Set.member` unvisited) neighbors
        let updatedNeighbors = (\(coord, elev) -> (coord, (fst elev, min (snd elev) (snd elevToVisit + 1)))) <$> unvisitedNeighbors :: [(Coordinate, Elevation)]
        let updatedNeighborsAsMap = Data.Map.fromAscList updatedNeighbors
        
        
        -- let newMap = updatedNeighborsAsMap `Data.Map.union` map
        let newMap = Prelude.foldl (\m pair -> Data.Map.update (\_ -> Just (snd pair)) (fst pair) m) map updatedNeighbors
        let newUnvisitedSet = coordToVisit `Data.Set.delete` unvisited
        
        dijkstras (newUnvisitedSet, newMap)



part1 input = do
    let rows = lines input
    let (map, (startPos, endPos)) = makeMap rows
    let (resSet, resMap, resFrontier) = dijkstrasFrontier (Data.Set.fromList $ fst <$> Data.Map.toList map, map, Data.Set.fromList [startPos])
    print $ endPos `Data.Map.lookup` resMap
    

part2 input = do
    let rows = lines input
    let (initMap, (givenStartPos, givenEndPos)) = makeMap rows
    let allAs = fst <$> Data.Map.toList (Data.Map.filter (\a -> fst a == 'a') initMap) :: [Coordinate]
    let results = (
            \a -> let map = (fixMap initMap a) in dijkstrasFrontier (Data.Set.fromList $ fst <$> Data.Map.toList (map), map, Data.Set.fromList [a])
            ) <$> allAs where 
                fixMap m startPos = Data.Map.insert startPos ('a', 0) (Data.Map.insert givenStartPos ('a', 2^31) m)
    let solves = (\(_, map, _)  -> givenEndPos `Data.Map.lookup` map) <$> results
    let dists = snd <$> catMaybes solves
    print $ minimum dists

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input
