import Text.Parsec
import Data.Either (rights)
import Data.Set
import Data.Map
import Data.Maybe (fromJust)
import Data.List (sortBy, permutations, nub)
import Data.Foldable (minimumBy)
import Data.Foldable (maximumBy)

fst3 :: (a,b,c) -> a
fst3 (x,_,_) = x
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

score :: AdjacencyMap -> Minutes -> Valve -> Valve -> Int
score adjMap timeLeft currentValve targetValve = do
    let path = fromJust (pathTo adjMap Data.Set.empty (name currentValve) (name targetValve))
    let distance = length path - 1
    flowRate targetValve * (timeLeft - distance - 1)

type Minutes = Int
type ValveName = String
type FlowRate = Int
type Valve = (ValveName, FlowRate)
name = fst
flowRate = snd
type ClosedValves = Set Valve
type AdjacencyMap = Map ValveName [ValveName]

buildMap :: [(Valve, [ValveName])] -> AdjacencyMap
buildMap valvesWithNeighbors = Data.Map.fromList ((\p -> (fst $ fst p, snd p)) <$> valvesWithNeighbors)

takeAction :: AdjacencyMap -> (Minutes, Valve, ClosedValves, Int) -> (Minutes, Valve, ClosedValves, Int) 
takeAction adjMap (minutesLeft, currentValve, closedValves, points) = do
    let possibleTargets = Data.Set.toList closedValves 
    let scores = score adjMap minutesLeft (currentValve) <$> possibleTargets
    let zipped = zip possibleTargets scores
    let move = maximumBy (\a b -> snd a `compare` snd b) zipped
    let moveDistance = length $ fromJust (pathTo adjMap Data.Set.empty (name currentValve) (name $ fst move))
    let newTime = minutesLeft - moveDistance 
    (newTime, fst move, fst move `Data.Set.delete` closedValves, points + newTime * (flowRate $ fst move))


prettyPrintResult :: (Minutes, Valve, ClosedValves, Int) -> String
prettyPrintResult (a, b, _, c) = show (a, b, c)

takeAction2 :: AdjacencyMap -> (Minutes, Valve, ClosedValves, Int) -> (Minutes, Valve, ClosedValves, Int) 
takeAction2 adjMap (minutesLeft, currentValve, closedValves, points) = do
    let possibleTargets = Data.Set.toList closedValves 
    let firstResults = (\t -> carryOut adjMap t (minutesLeft, currentValve, closedValves, points)) <$> possibleTargets
    let secondResults = (\(mleft, curv, closv, scor) -> (\x -> carryOut adjMap  x (mleft, curv, closv, scor)) <$> Data.Set.toList closv ) <$> firstResults

    let z = zip (prettyPrintResult <$> firstResults) (fmap prettyPrintResult <$> secondResults)
    error $ concat $ (\(fst, seconds) -> fst ++ "\n" ++ (unlines $ ("\t" ++ ) <$> seconds)) <$> z
    -- let k =  ((\lst -> fmap prettyPrintResult lst) <$> secondResults)
    -- error $ unlines $  concat k


takeActionbetter :: AdjacencyMap -> (Minutes, Valve, ClosedValves, Int) -> (Minutes, Valve, ClosedValves, Int) 
takeActionbetter adjMap (minutesLeft, currentValve, closedValves, points) = do
    let possibleTargets = Data.Set.toList closedValves 
    let pairs = do
            firsts <- (possibleTargets)
            seconds <- (possibleTargets)
            -- thirds <- (possibleTargets)
            -- fourths <- (possibleTargets)
            -- fifths <- (possibleTargets)
            -- sixths <- (possibleTargets)
            -- sevenths <- (possibleTargets)
            Prelude.filter ((2== ) . length . nub) (return [firsts, seconds])
    let results = (\x -> recursivelyCarryOut adjMap x (minutesLeft, currentValve, closedValves, points)) <$> pairs
    let z = zip (fmap name <$>pairs) (prettyPrintResult <$> results)
    let points = (\(_, _, _, pts) -> pts) 
    error $ show $ maximumBy (\t1 t2 -> points (snd t1) `compare` points (snd t2)) (zip pairs results)
    -- error $ unlines $ show <$> z
    
    -- let firstResults = (\t -> carryOut adjMap t (minutesLeft, currentValve, closedValves, points)) <$> possibleTargets
    -- let secondResults = (\(mleft, curv, closv, scor) -> (\x -> carryOut adjMap  x (mleft, curv, closv, scor)) <$> Data.Set.toList closv ) <$> firstResults

    -- let z = zip (prettyPrintResult <$> firstResults) (fmap prettyPrintResult <$> secondResults)
    -- error $ concat $ (\(fst, seconds) -> fst ++ "\n" ++ (unlines $ ("\t" ++ ) <$> seconds)) <$> z


carryOut :: AdjacencyMap -> Valve -> (Minutes, Valve, ClosedValves, Int) -> (Minutes, Valve, ClosedValves, Int)
carryOut adjMap targetValve (minutesLeft, currentValve, closedValves, points) = do
    let path = pathTo adjMap Data.Set.empty (name currentValve) (name targetValve)
    let timeLeftAfterOpening = minutesLeft - length (fromJust path)
    (timeLeftAfterOpening, targetValve, targetValve `Data.Set.delete` closedValves, points + (flowRate targetValve * timeLeftAfterOpening))


recursivelyCarryOut :: AdjacencyMap -> [Valve] -> (Minutes, Valve, ClosedValves, Int) -> (Minutes, Valve, ClosedValves, Int)
recursivelyCarryOut adjMap [] (timeLeft, currValve, closedValves, points) = (timeLeft, currValve, closedValves, points)  
recursivelyCarryOut adjMap (nextValve:rest) (timeLeft, currValve, closedValves, points) = recursivelyCarryOut adjMap rest (carryOut adjMap nextValve (timeLeft, currValve, closedValves, points))  

part1 input = do
    let valves = rights $ (runParser parseValve () "") <$> (lines input)
    let adjMap = buildMap valves
    let closedValves = Data.Set.fromList (fst <$> valves) :: ClosedValves
    print $ takeActionbetter adjMap (30, (fst . head) valves, closedValves, 0)




main = do
    input <- readFile "input.txt"
    part1 input