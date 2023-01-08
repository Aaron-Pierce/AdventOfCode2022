{-# LANGUAGE FlexibleContexts #-}
import Text.Parsec (runParser, option, many)
import Text.Parsec.Char (char, digit)
import Data.Either (rights)
import Data.Set (Set, member, empty, insert, fromList, null, notMember, unions, elemAt, difference, singleton, size, toList)
import Data.List (nub)

type Coordinate = (Int, Int, Int)
cadd :: Coordinate -> Coordinate -> Coordinate
cadd (c1x, c1y, c1z) (c2x, c2y, c2z) = (c1x + c2x, c1y + c2y, c1z + c2z)

parseNegativeSign = do
    char '-'
    return (-1)

parseNumber = do
    signNum <- option 1 parseNegativeSign
    digits <- many digit
    return $ (read digits :: Int) * signNum

parsePoint = do
    x <- parseNumber
    char ','
    y <- parseNumber
    char ','
    z <- parseNumber
    return (x, y, z)


getNeighbors :: Coordinate -> [Coordinate]
getNeighbors point = (point `cadd`) <$> [(-1, 0, 0), (1, 0, 0), (0, -1, 0), (0, 1, 0), (0, 0, -1), (0, 0, 1)]

computeSurfaceArea :: Int -> Set Coordinate -> [Coordinate] -> Int
computeSurfaceArea currentArea currentShape [] = currentArea
computeSurfaceArea currentArea currentShape (point:rest) = do
    let potentialNeighbors = getNeighbors point
    let occupiedNeighbors = (`Data.Set.member` currentShape) <$> potentialNeighbors 
    let newArea = currentArea + 6 - (2 * length (filter id occupiedNeighbors))
    computeSurfaceArea newArea (Data.Set.insert point currentShape) rest

part1 input = do
    let coordinates = rights $ runParser parsePoint () "inputLine" <$> lines input
    let surfaceArea = computeSurfaceArea 0 Data.Set.empty coordinates
    print surfaceArea

insideLegalArea :: Coordinate -> Bool
insideLegalArea (px, py, pz) = and $ (\c -> c >= 0 && c <= 20) <$> [px, py, pz]

type IsExteriorExposed = Bool
floodFill :: [Coordinate] -> Set Coordinate -> Set Coordinate -> (Set Coordinate, IsExteriorExposed)
floodFill fringe visited shape 
    | Prelude.null fringe = (visited, (20, 20, 20) `elem` visited)
    | otherwise = do
        let neighboringAir = filter (`Data.Set.notMember` shape) (concatMap getNeighbors fringe)
        let unvisitedNeighbors = nub $ filter (\p -> insideLegalArea p && p `Data.Set.notMember` visited) neighboringAir
        let newVisitedSet = foldl (flip Data.Set.insert) visited unvisitedNeighbors
        floodFill unvisitedNeighbors newVisitedSet shape

findInteriorSegments :: Set Coordinate -> Set Coordinate -> [Set Coordinate]
findInteriorSegments searchSpace shape 
    | Data.Set.null searchSpace = []
    | otherwise = do
        let p = Data.Set.elemAt 0 searchSpace
        let (interiorArea, isExposed) = floodFill [p] (Data.Set.singleton p) shape
        let newSearchSpace = searchSpace `Data.Set.difference` interiorArea
        let next = findInteriorSegments newSearchSpace shape
        if not isExposed
            then interiorArea:next
            else next

part2 input = do
    let coordinates = rights $ runParser parsePoint () "inputLine" <$> lines input
    let shape = Data.Set.fromList coordinates
    let possibleArea = [(x, y, z) | x <- [0..20], y <- [0..20], z <- [0..20]]
    let interiorSegments = findInteriorSegments (Data.Set.fromList possibleArea `Data.Set.difference` shape) shape
    let interiorSegmentAreas = computeSurfaceArea 0 Data.Set.empty <$> (Data.Set.toList <$> interiorSegments)
    let totalArea = computeSurfaceArea 0 Data.Set.empty coordinates
    print interiorSegmentAreas
    print $ totalArea - sum interiorSegmentAreas

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input
