import Data.List.Split
import Data.Char (isSpace)
import Data.List (dropWhileEnd, sort)
import Data.Maybe (fromJust, isNothing, fromMaybe)

trim = dropWhileEnd isSpace . dropWhile isSpace

data Monkey = Monkey {
    inventory :: [Int],
    operation :: Int -> Int,
    test      :: Int -> Bool,
    divisor   :: Int,
    trueDestination  :: Int,
    falseDestination :: Int,
    numInspected     :: Int
}

constructMonkeyFunction :: (Int -> Int -> Int) -> Maybe Int -> Maybe Int -> (Int -> Int)
constructMonkeyFunction operator maybe_lhs maybe_rhs x = fromMaybe x maybe_lhs `operator` fromMaybe x maybe_rhs

parseMonkey :: String -> Monkey
parseMonkey monkeyBlock = do
    let input_lines = lines monkeyBlock
    let items = read <$> splitOn "," (drop 16 $ trim (input_lines !! 1)) :: [Int]

    let operationParts = splitOn " " $ drop 17 (trim $ input_lines !! 2)
    let operatorFunction = case operationParts !! 1 of
            "*" -> (*)
            "+" -> (+)
            "-" -> (-)
            "/" -> div
    let resolveOperand x = case x of
            "old" -> Nothing
            _ -> Just (read x :: Int)
    let monkeyFunction = constructMonkeyFunction operatorFunction (resolveOperand $ head operationParts) (resolveOperand $ last operationParts)


    let testDivisor = read (drop 19 $ trim $ input_lines !! 3) :: Int
    let testFn = (0 == ) . (`mod` testDivisor)  

    let trueDest  = read (drop 25 $ trim $ input_lines !! 4) :: Int
    let falseDest = read (drop 26 $ trim $ input_lines !! 5) :: Int
    

    Monkey items monkeyFunction testFn testDivisor trueDest falseDest 0

type WorryFunction = (Int -> (Int -> Int) -> Int)

performRound :: WorryFunction -> [Monkey] -> [Monkey]
performRound worryFunction monkeys = foldl (performTurn worryFunction) monkeys [0..(length monkeys - 1)]

performTurn :: WorryFunction -> [Monkey] -> Int -> [Monkey]
performTurn worryFunction monkeys monkeyIndex = do
    let newMonkeys = foldl (\monkeyList item -> let (newWorry, destination) = inspectItem item (monkeys !! monkeyIndex) worryFunction in throwItem newWorry destination monkeyList) monkeys (inventory $ monkeys !! monkeyIndex)
    let clearedInventory = (\(ind, mky) -> if ind /= monkeyIndex then mky else 
            Monkey [] (operation mky) (test mky) (divisor mky) (trueDestination mky) (falseDestination mky) (numInspected mky + length (inventory mky))
            ) <$> zip [0..] newMonkeys
    clearedInventory

inspectItem :: Int -> Monkey -> WorryFunction -> (Int, Int)
inspectItem item monkey worryFunction = do
    let newWorry = worryFunction item (operation monkey)
    let destination = if test monkey newWorry then trueDestination monkey else falseDestination monkey
    (newWorry, destination) 

appendItem :: Int -> Monkey -> Monkey
appendItem item monkey = Monkey (inventory monkey ++ [item]) (operation monkey) (test monkey) (divisor monkey) (trueDestination monkey) (falseDestination monkey) (numInspected monkey)

throwItem :: Int -> Int -> [Monkey] -> [Monkey]
throwItem itemWorry destinationIndex monkeys = (\(ind, mky) -> if ind /= destinationIndex then mky else appendItem itemWorry mky) <$> zip [0..] monkeys

part1Worry :: Int -> (Int -> Int) -> Int
part1Worry worryLevel monkeyFunction = monkeyFunction worryLevel `div` 3

part2Worry :: Int -> Int -> (Int -> Int) -> Int
part2Worry divisor worryLevel monkeyFunction = monkeyFunction worryLevel `mod` divisor

part1 input = do
    let input_monkeys = splitOn "\n\n" input
    let monkeys = parseMonkey <$> input_monkeys
    let finalMonkeys = iterate (performRound part1Worry) monkeys !! 20
    let sortedInspected = reverse $ sort $ numInspected <$> finalMonkeys
    let monkeyBusiness = (sortedInspected !! 0) * (sortedInspected !! 1)
    print monkeyBusiness


part2 input = do
    let input_monkeys = splitOn "\n\n" input
    let monkeys = parseMonkey <$> input_monkeys
    let divisorProduct = product (divisor <$> monkeys)
    let finalMonkeys = iterate (performRound (part2Worry divisorProduct)) monkeys !! 10000
    let sortedInspected = reverse $ sort $ numInspected <$> finalMonkeys
    let monkeyBusiness = (sortedInspected !! 0) * (sortedInspected !! 1)
    print monkeyBusiness

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input
