import Data.List (isPrefixOf, find)
import Data.Maybe (fromJust)
import Data.Typeable (typeOf)

data FileTree = Dir {
    name :: String,
    children :: [FileTree]
} | File {
    name :: String,
    size :: Int
} 

addChildren :: FileTree -> [String] -> [FileTree] -> FileTree
addChildren root [] new_children = Dir {name = name root, children = children root ++ new_children}
addChildren root (nextDir:remainingPath) new_children = Dir {
    name = name root, 
    children = 
        (\child -> if name child == nextDir then addChildren child remainingPath new_children else child) 
        <$> children root
    }


treeSize :: FileTree -> Int
treeSize File{size = s} = s
treeSize Dir{children = c} = sum $ treeSize <$> c


flattenTree :: FileTree -> [FileTree]
flattenTree File{name=n, size=s} = [File{name=n, size=s}]
flattenTree Dir{name=n, children=c} = Dir{name=n, children=c}:concatMap flattenTree c


buildTree :: FileTree -> [[String]] -> FileTree
buildTree rootNode commandResultGroups = fst (foldl updateTree (rootNode, []) commandResultGroups) where
        updateTree :: (FileTree, [String]) -> [String] -> (FileTree, [String])
        updateTree (root, cwd) (command:results) = if null results
            then do
                let w = words command
                let dirName = w !! 2
                if dirName == ".."
                    then (root, init cwd)
                    else (root, cwd ++ [dirName])
            else (addChildren root cwd (toChildren results), cwd)

        toChildren :: [String] -> [FileTree]
        toChildren entries = (\entry -> do
                let w = words entry
                if head w == "dir"
                    then Dir {name = last w, children = []}
                    else File {name = last w, size = read $ head w}
            ) <$> entries

part1 input = do
    let input_lines = lines input
    let grouped = tail $ foldl (\acc el -> if "$" `isPrefixOf` el
        then acc ++ [[el]]
        else init acc ++ [last acc ++ [el]]
            ) [] input_lines
    
    let rootNode = Dir {name = "/", children = []}

    let tree = buildTree rootNode grouped
    let flattened = flattenTree tree
    let dirs = filter isDir flattened where
        isDir Dir{} = True
        isDir File{} = False
    let dirSizes = treeSize <$> dirs
    let filteredDirSizes = filter (<= 100000) dirSizes
    print $ sum filteredDirSizes


part2 input = do
    let input_lines = lines input
    let grouped = tail $ foldl (\acc el -> if "$" `isPrefixOf` el
        then acc ++ [[el]]
        else init acc ++ [last acc ++ [el]]
            ) [] input_lines
    
    let rootNode = Dir {name = "/", children = []}

    let tree = buildTree rootNode grouped
    let flattened = flattenTree tree
    let dirs = filter isDir flattened where
        isDir Dir{} = True
        isDir File{} = False
    let dirSizes = treeSize <$> dirs

    let totalUsage = treeSize tree
    let remaining = 70000000 - totalUsage
    let needToFree = 30000000 - remaining
    let largerThanWhatWeNeed = filter (>= needToFree) dirSizes
    print $ minimum largerThanWhatWeNeed

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input