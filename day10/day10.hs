import Data.List.Split (chunksOf)

type Command = (String, Int)
type ExecutionQueue = [(Command, Int)]
type AccumulatorValue = Int
type AccumulatorHistory = [AccumulatorValue]
type CycleCount = Int
type CPUState = (AccumulatorHistory, CycleCount)

parseCommand :: String -> (String, Int)
parseCommand line = (take 4 line, let param = drop 5 line in if param == "" then 0 else read param)


runCommand :: CPUState -> Command -> CPUState
runCommand (accHistory, cycleCount) command = case fst command of
    "noop" -> (accHistory ++ [last accHistory], cycleCount + 1)
    "addx" -> (accHistory ++ [last accHistory, last accHistory + snd command], cycleCount + 2)


part1 input = do
    let commands = parseCommand <$> lines input
    let score = foldl runCommand ([1], 0) commands 
    let indices = [20, 60, 100, 140, 180, 220]
    let strength = sum $ (\i -> i * (fst score !! (i-1))) <$> indices
    print strength

    

part2 input = do
    let commands = parseCommand <$> lines input
    let accValues = fst $ foldl runCommand ([1], 0) commands
    let scanLines = chunksOf 40 (init accValues)
    let pixels = (\line -> fmap (\(accValue, ind) -> if abs (accValue - ind) <= 1 then "#" else ".") (zip line [0..])) <$> scanLines
    putStrLn <$> unlines $ unwords <$> pixels

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input