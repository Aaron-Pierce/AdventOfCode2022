import Data.List (nub)

type Point = (Int, Int)
type Move = (Char, Int)

padd :: Point -> Point -> Point
p1 `padd` p2 = (fst p1 + fst p2, snd p1 + snd p2)

psub :: Point -> Point -> Point
p1 `psub` p2 = (fst p1 - fst p2, snd p1 - snd p2)

pmul :: Point -> Point -> Point
p1 `pmul` p2 = (fst p1 * fst p2, snd p1 * snd p2)

pabs :: Point -> Point
pabs p1 = (abs $ fst p1, abs $ snd p1)

near :: Point -> Point -> Bool
near p1 p2 = (abs (fst p1 - fst p2) <= 1) && (abs (snd p1 - snd p2) <= 1)

moveToDelta :: Move -> Point
moveToDelta = movePoint (0, 0)


-- Pretend a knot is at the middle of this matrix.
-- We just moved the knot before it to some position
-- Take its relative position to the knot after it,
-- use that relative position to index into this matrix,
-- and move the 'tail' knot by that amount.

-- For example, we move the first knot such that it is 2 units
-- to the left and 1 unit above the second knot.
-- We start from the center of this matrix, go 2 units to the left
-- and 1 unit up, so we should shift the second knot 1 unit to the left
-- and 1 unit up.
moveMap = [
        [ (-1, 1),  (-1, 1),   (0, 1),   (1, 1),   (1, 1)],
        [ (-1, 1),   (0, 0),   (0, 0),   (0, 0),   (1, 1)],
        [ (-1, 0),   (0, 0),   (0, 0),   (0, 0),   (1, 0)],
        [(-1, -1),   (0, 0),   (0, 0),   (0, 0),  (1, -1)],
        [(-1, -1), (-1, -1),  (0, -1),  (1, -1),  (1, -1)]
    ]


type KnotsWithHistory = ([Point], [Point])

-- The user should use this function to apply a move to the rope, and it'll keep track of the history of the tail knot.
moveRope :: KnotsWithHistory -> Move -> KnotsWithHistory
moveRope (knots, history) move 
    | snd move > 1 = moveRope (moveRope (knots, history) (fst move, snd move - 1)) (fst move, 1)
    | otherwise = do
            let newKnots = _pullRope knots (moveToDelta move)
            (newKnots, (last knots):history)


-- Internal method used to actually move the rope, assumes that the delta given to move the 
-- head node has no component with magnitude greater than 1.
-- So it should be in [{-1, 0, 1}, {-1, 0, 1}]
_pullRope :: [Point] -> Point -> [Point]
_pullRope [] _ = []
_pullRope [knot] delta = [knot `padd` delta]
_pullRope (h:t:rest) delta = (h `padd` delta):(_pullRope (t:rest) (deriveDelta h t delta))



-- Keeping this here for posterity:
-- This disgusting function made me realize that moves were more constrained than I thought,
-- namely that the head and tail are never more than 1 unit apart in either axis,
-- and cant move more than 1 unit in either axis, so a 5x5 grid covers every possible
-- configuration of ending positions of the head relative to the tail,
-- so we can just enumerate them and where to move the tail as a result.
-- How cool! (that's stored in moveMap)

-- Begin gnarly function which is no longer necessary:
        -- Given the position of a knot (the 'head'), the knot after it (the 'tail'), and the delta by which the head is to be moved
        -- (at most 1 unit in both directions), figure out what delta to apply to the tail knot.
        -- deriveDelta :: Point -> Point -> Point -> Point
        -- deriveDelta headPos tailPos headDelta
        --     | tailPos `near` finalHeadPos = (0, 0) -- If the tail is close enough to the head's final position, do nothing
        --     | (headPos `psub` tailPos) == headDelta = headDelta -- If the tail is adjacent to the head in the direction of movement, then just move it along with the head
        --     | uncurry (*) diffv == 0 = (signum $ fst diffv, signum $ snd diffv)  -- If the head moves in line with the tail on some axis, shift the tail up along the axis on which they just aligned
        --     | otherwise = headDelta `padd` (((1, 1) `psub` pabs headDelta) `pmul` (headPos `psub` tailPos)) -- Otherwise, the head either moves diagonally away from the tail, and isn't in the same column or row,
        --                                                                                                     -- so we move the tail by the same shift, 
        --                                                                                                     -- or it is diagonally adjacent to the tail but moves strictly horizontally or vertically. 
        --                                                                                                     -- In this case, shift the tail with the head, and then make the tail not diagonal anymore.
        --                                                                                                     -- So if the head is top right of the tail and moves right, we move the tail right, and then up 
        --                                                                                                     -- to make it no longer diagonal.
        --     where finalHeadPos = headPos `padd` headDelta
        --           diffv = finalHeadPos `psub` tailPos
-- End gnarly function which is no longer necessary


-- Given the position of a knot and the delta we will move it by, 
-- derive the change we need to apply to the knot after it
deriveDelta :: Point -> Point -> Point -> Point
deriveDelta headPos tailPos headDelta 
        = (moveMap !! (2 - snd finalDiff)) !! (2 + fst finalDiff)
        where finalDiff = (headPos `padd` headDelta) `psub` tailPos

movePoint :: Point -> Move -> Point
movePoint point ('U', n) = (fst point, snd point + n)
movePoint point ('D', n) = (fst point, snd point - n)
movePoint point ('L', n) = (fst point - n, snd point)
movePoint point ('R', n) = (fst point + n, snd point)
movePoint point (m, n) = error $ "Unknown m " ++ show m

parseMove :: String -> Move
parseMove line = (head line, read $ drop 2 line)

part1 input = do


    let moves = parseMove <$> lines input

    let (knotPositions, history) = foldl moveRope (replicate 2 (0, 0), []) moves
    let tailPositions = reverse $ (last knotPositions):history
    print $ length $ nub $ tailPositions


part2 input = do
    let moves = parseMove <$> lines input

    let (knotPositions, history) = foldl moveRope (replicate 10 (0, 0), []) moves
    let tailPositions = reverse $ (last knotPositions):history
    print $ length . nub $ tailPositions

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input