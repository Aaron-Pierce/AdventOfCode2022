import Data.Map (empty, map, Map, lookup, insert, null, keysSet, keys)
import Data.Foldable (maximumBy)
import Data.List (minimumBy, sortBy, groupBy)
import Data.Maybe (isNothing)

type Coordinate = (Int, Int)

cadd :: Coordinate -> Coordinate -> Coordinate
x `cadd` y = (fst x + fst y, snd x + snd y)
x `csub` y = (fst x - fst y, snd x - snd y)

data Material = Air | Rock | FallingRock deriving (Show, Eq)

type Board = Map Coordinate Material
type PieceIndex = Int
type MoveString = String
type MoveIndex = Int

dropPiece :: (Board, PieceIndex, MoveString, MoveIndex) -> (Board, PieceIndex, MoveString, MoveIndex)
dropPiece (board, pieceIndex, moveString, moveIndex) = do
    let (newBoard, newMoveIndex) = stepPiece board pieceIndex (getStartPosition board `csub` (0, (getHeight pieceIndex) - 1)) moveString moveIndex
    let newPieceIndex = (pieceIndex + 1) `mod` 5
    (newBoard, newPieceIndex, moveString, newMoveIndex)

stepPiece :: Board -> PieceIndex -> Coordinate -> MoveString -> MoveIndex -> (Board, MoveIndex)
stepPiece board pieceIndex rootCoordinate moves moveIndex = do
    let sideMove = moveSide board pieceIndex rootCoordinate moves moveIndex
    let downMove = moveDown board pieceIndex sideMove
    if downMove == sideMove
        then (placePiece board pieceIndex downMove, (moveIndex + 1) `mod` length moves)
        else stepPiece board pieceIndex downMove moves ((moveIndex + 1) `mod` length moves)

placePiece :: Board -> PieceIndex -> Coordinate -> Board
placePiece board pieceIndex rootPosition = foldl (\b e -> Data.Map.insert e Rock b)  board (getCoordinates pieceIndex rootPosition) 

moveDown :: Board -> PieceIndex -> Coordinate -> Coordinate
moveDown board pieceIndex rootPosition = makeMoveIfValid board pieceIndex rootPosition (0, 1)

moveSide :: Board -> PieceIndex -> Coordinate -> MoveString -> MoveIndex -> Coordinate
moveSide board pieceIndex rootPosition moveString moveIndex 
    | moveString !! moveIndex == '<' = makeMoveIfValid board pieceIndex rootPosition (-1, 0)
    | moveString !! moveIndex == '>' = makeMoveIfValid board pieceIndex rootPosition (1, 0)


makeMoveIfValid :: Board -> PieceIndex -> Coordinate -> Coordinate -> Coordinate
makeMoveIfValid board pieceIndex rootPos move = do if validPiecePlacement board pieceIndex (rootPos `cadd` move) then rootPos `cadd` move else rootPos

validPiecePlacement :: Board -> PieceIndex -> Coordinate -> Bool
validPiecePlacement board pieceIndex coordinate = 
    all (== Nothing) ((`Data.Map.lookup` board) <$> getCoordinates pieceIndex coordinate)
        && all withinBoard (getCoordinates pieceIndex coordinate)

getCoordinates :: PieceIndex -> Coordinate -> [Coordinate]
getCoordinates pieceIndex root
    | pieceIndex == 0 = (root `cadd`) <$> [(0, 0), (1, 0), (2, 0), (3, 0)]
    | pieceIndex == 1 = (root `cadd`) <$> [(1, 0), (0, 1), (1, 1), (2, 1), (1, 2)]
    | pieceIndex == 2 = (root `cadd`) <$> [(2, 0), (2, 1), (2, 2), (1, 2), (0, 2)]
    | pieceIndex == 3 = (root `cadd`) <$> [(0, 0), (0, 1), (0, 2), (0, 3)]
    | pieceIndex == 4 = (root `cadd`) <$> [(0, 0), (0, 1), (1, 0), (1, 1)]
    | otherwise = error $ "Invalid piece index " ++ show pieceIndex

withinBoard :: Coordinate -> Bool
withinBoard (x, y) = x >= 0 && x <= 6 && y < 0

getStartPosition :: Board -> Coordinate
getStartPosition board
    | Data.Map.null board = (2, -4)
    | otherwise = (2, (snd $ minimumBy (\a b -> snd a `compare` snd b) (Data.Map.keys board)) - 4)

getHeight :: PieceIndex -> Int
getHeight pieceIndex = do
    let highestPoint = minimumBy (\p1 p2 -> snd p1 `compare` snd p2) (getCoordinates pieceIndex (0, 0))
    let lowestPoint = maximumBy (\p1 p2 -> snd p1 `compare` snd p2) (getCoordinates pieceIndex (0, 0))
    snd lowestPoint - snd highestPoint + 1

boardHeight :: Board -> Int
boardHeight board = do
    let points = Data.Map.keys board
    let highestPoint = minimumBy (\p1 p2 -> snd p1 `compare` snd p2) points
    let lowestPoint = maximumBy (\p1 p2 -> snd p1 `compare` snd p2) points
    snd lowestPoint - snd highestPoint + 1

hasFullRow :: Board -> Maybe Int
hasFullRow board = do
    let points = sortBy (\a b -> snd a `compare` snd b) (Data.Map.keys board)
    let groupedByYValue = groupBy (\x y -> snd x == snd y) points
    let lengthsAndYVals = (\list -> (length list, snd $ head list)) <$> groupedByYValue
    let fullRows = filter (\(length, yVal) -> length >= 7) lengthsAndYVals
    if Prelude.null fullRows then Nothing else Just (snd $ last fullRows)

isFullRow :: Board -> Int -> Bool
isFullRow board yVal = (not . any isNothing) ((`Data.Map.lookup` board) <$> [(x, yVal) | x <- [0..7]])

part1 input = do
    let results = iterate dropPiece (Data.Map.empty, 0, input, 0) !! 2022
    let finishedBoard = (\(x, _, _, _) -> x) results
    print $ boardHeight finishedBoard

part2 input = do
    let results = iterate dropPiece (Data.Map.empty, 0, input, 0) !! 100000
    let finishedBoard = (\(x, _, _, _) -> x) results
    print $ boardHeight finishedBoard
    print $ hasFullRow finishedBoard

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input