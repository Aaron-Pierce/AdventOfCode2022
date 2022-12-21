{-# LANGUAGE BinaryLiterals#-}


import Text.Parsec
import Data.Either (rights)
import Data.Set (Set, insert, notMember, empty, toList, delete, null, fromList, size)
import Data.Map (Map, fromList, insert, lookup, empty, keys)
import Data.Maybe (fromJust, isNothing)
import Data.List (sortBy, permutations, nub)
import Data.Foldable (find, maximumBy, minimumBy)
import Data.Bits

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x

snd3 :: (a,b,c) -> b
snd3 (_,x,_) = x

thd :: (a,b,c) -> c
thd (_,_,x) = x
parseValve = do
    string "Valve "
    valveName <- many letter
    string " has flow rate="
    flowRate <- many digit
    string "; "
    string "tunnel"
    optional $ char 's'
    char ' '
    string "lead"
    optional $ char 's'
    string " to valve"
    optional $ char 's'
    char ' '
    neighbors <- sepBy (many letter) (string ", ")
    return ((valveName, read flowRate :: Int), neighbors)


pathTo :: AdjacencyMap -> Set ValveName -> ValveName -> ValveName -> Maybe [ValveName]
pathTo adjMap visitedSet v1 v2
    | v1 == v2 = Just [v2]
    | Prelude.null unvisitedNeighbors = Nothing 
    | v2 `elem` unvisitedNeighbors = Just [v1, v2]
    | otherwise = let {
            pathsThroughNeighbors = (\neighbor -> pathTo adjMap (Data.Set.insert v1 visitedSet) neighbor v2) <$> unvisitedNeighbors;
            validPaths = fromJust <$> Prelude.filter (not . Prelude.null) pathsThroughNeighbors
        } in if (Prelude.null validPaths) then Nothing else Just $ v1 : (minimumBy (\a b -> length a `compare` length b) validPaths)
    where unvisitedNeighbors = Prelude.filter (`Data.Set.notMember` visitedSet) (fromJust (v1 `Data.Map.lookup` adjMap))


type Minutes = Int
type TimeLeft = Minutes
type ValveName = String
type FlowRate = Int
type Valve = (ValveName, FlowRate)
name = fst
flowRate = snd
type ClosedValves = Set Valve
type AdjacencyMap = Map ValveName [ValveName]
type ShortestPathMap = Map (ValveName, ValveName) [ValveName]

buildMap :: [(Valve, [ValveName])] -> AdjacencyMap
buildMap valvesWithNeighbors = Data.Map.fromList ((\p -> (fst $ fst p, snd p)) <$> valvesWithNeighbors)

buildShortestPathMap :: AdjacencyMap -> ShortestPathMap
buildShortestPathMap adjMap = Prelude.foldl (\spMap pair -> Data.Map.insert pair (fromJust $ uncurry (pathTo adjMap Data.Set.empty) pair) (spMap)) Data.Map.empty [(v1, v2) | v1 <- Data.Map.keys adjMap, v2 <- Data.Map.keys adjMap]


solve :: AdjacencyMap -> ShortestPathMap -> ClosedValves -> TimeLeft -> Valve -> ([ValveName], Int)
solve adjMap pathMap closedValves minutesLeft currentValve
    | minutesLeft <= 0 = ([name currentValve], 0)
    | Data.Set.null closedValves = ([name currentValve], minutesLeft * flowRate currentValve)
    | otherwise = do 
        let solutions = (\v -> solve adjMap pathMap (Data.Set.delete v closedValves) (minutesLeft - length (fromJust $ (name currentValve, name v) `Data.Map.lookup` pathMap)) v) <$> Data.Set.toList closedValves
        let bestSolution = maximumBy (\s1 s2 -> snd s1 `compare` snd s2) solutions
        (name currentValve : fst bestSolution, snd bestSolution + minutesLeft * snd currentValve)


timeAfterMove :: ShortestPathMap -> TimeLeft -> ValveName -> ValveName -> Minutes
timeAfterMove pathMap currentTime startValve endValve = do
    let lookup = (startValve, endValve) `Data.Map.lookup` pathMap
    
    let timeLeft = if isNothing lookup then error ("Couldn't find " ++ startValve ++ "->" ++ endValve) else currentTime - length (fromJust lookup)
    max timeLeft 0

solve2 :: AdjacencyMap -> ShortestPathMap -> ClosedValves -> (TimeLeft, TimeLeft) -> (Valve, Valve) -> ([ValveName], [ValveName], Int)
solve2 adjMap pathMap closedValves (myMinutes, elephantMinutes) (myValve, elephantValve)
    | Data.Set.null newClosedValves = ([name myValve], [name elephantValve], (max 0 myMinutes) * snd myValve + (max 0 elephantMinutes) * snd elephantValve)
    | Data.Set.size newClosedValves == 1 = do
        let baseScore = (myMinutes * flowRate myValve) + (elephantMinutes * flowRate elephantValve)
        let remainingValve = head (Data.Set.toList closedValves)
        let myOpeningTime = myMinutes - length (fromJust $ (name myValve, name remainingValve) `Data.Map.lookup` pathMap)
        let elephantOpeningTime = elephantMinutes - length (fromJust $ (name elephantValve, name remainingValve) `Data.Map.lookup` pathMap)
        if myOpeningTime > elephantOpeningTime 
            then ([name myValve, name remainingValve],                      [name elephantValve], baseScore + (myOpeningTime * flowRate remainingValve))
            else ([name myValve],                      [name elephantValve, name remainingValve], baseScore + (elephantOpeningTime * flowRate remainingValve))
    | myMinutes <= 0 && elephantMinutes <= 0 = ([name myValve], [name elephantValve], 0)
    | otherwise = do
        let nextValves = [(v1, v2) | v1 <- Data.Set.toList newClosedValves, v2 <- Data.Set.toList newClosedValves, name v1 /= name v2 ]

        let possibleNextSteps = 
                (\(myNextValve, elephantNextValve) -> 
                    solve2 
                        adjMap 
                        pathMap 
                        newClosedValves 
                        (
                            timeAfterMove pathMap myMinutes       (name myValve)       (name myNextValve),
                            timeAfterMove pathMap elephantMinutes (name elephantValve) (name elephantNextValve)
                        ) 
                        (myNextValve, elephantNextValve)
                ) 
                <$> nextValves

        
        
        let bestPossibility = maximumBy (\s1 s2 -> thd s1 `compare` thd s2) (if Prelude.null possibleNextSteps then error (show newClosedValves) else possibleNextSteps)
        (name myValve : fst3 bestPossibility, name elephantValve : snd3 bestPossibility, thd bestPossibility + ((max 0 myMinutes) * flowRate myValve) + ((max 0 elephantMinutes) * flowRate elephantValve))
    where 
        newClosedValves = elephantValve `Data.Set.delete` (myValve `Data.Set.delete` closedValves)


part1 input = do
    let valvesWithNeighbors = rights $ runParser parseValve () "" <$> lines input
    let adjMap = buildMap valvesWithNeighbors
    let shortestPathMap = buildShortestPathMap adjMap
    let nonZeroValves = Prelude.filter (\a -> snd a > 0) (fst <$> valvesWithNeighbors)
    let startValve = fromJust $ find (\v -> name v == "AA") (fst <$> valvesWithNeighbors)
    let closedValves = Data.Set.fromList nonZeroValves
    let solution = solve adjMap shortestPathMap (startValve `Data.Set.delete` closedValves) 30 startValve
    print $ solution

part2 input = do
    let valvesWithNeighbors = rights $ runParser parseValve () "" <$> lines input
    let adjMap = buildMap valvesWithNeighbors
    let shortestPathMap = buildShortestPathMap adjMap
    let nonZeroValves = Prelude.filter (\a -> snd a > 0) (fst <$> valvesWithNeighbors)
    let startValve = fromJust $ find (\v -> name v == "AA") (fst <$> valvesWithNeighbors)
    let closedValves = Data.Set.fromList nonZeroValves
    let solution = solve2 adjMap shortestPathMap (startValve `Data.Set.delete` closedValves) (14, 14) (startValve, startValve)
    print $ solution

bisect:: [a] -> Int -> ([a], [a])
bisect list mask = do
    let left = filter (\(ind, el) -> (mask .&. (2^ind)) > 0) (zip [0..] list)
    let right = filter (\(ind, el) -> ((complement mask) .&. (2^ind)) > 0) (zip [0..] list)
    (snd <$> left, snd <$> right)

part2a input = do
    let valvesWithNeighbors = rights $ runParser parseValve () "" <$> lines input
    let adjMap = buildMap valvesWithNeighbors
    let shortestPathMap = buildShortestPathMap adjMap
    let startValve = fromJust $ find (\v -> name v == "AA") (fst <$> valvesWithNeighbors)
    let nonZeroValves = Prelude.filter (\a -> snd a > 0) (fst <$> valvesWithNeighbors)
    let bisections = bisect nonZeroValves <$> [0..(2^length nonZeroValves)]
    let closedSets = (\(l, r) -> (Data.Set.fromList (l), Data.Set.fromList (r))) <$> bisections
    print $ length closedSets
    let solutions = (\(c1, c2) -> 
            (solve adjMap shortestPathMap c1 26 startValve,
             solve adjMap shortestPathMap c2 26 startValve)) <$> (closedSets)
    print $ length solutions
    let scores = (\(a, b) -> snd a + snd b) <$> solutions
    let bestSolution = maximum scores
    print bestSolution
main = do
    input <- readFile "input.txt"
    part1 input
    part2a input