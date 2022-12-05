{-# LANGUAGE OverloadedStrings #-}
import Data.List (intercalate)
import Data.List.Split
import Data.List (transpose, stripPrefix)
import Data.Text (strip, pack, null, unwords, unlines, unpack, Text, stripSuffix, stripPrefix, split, splitOn, concat)
import Data.Maybe (fromJust)
import Data.Text.Read (decimal)
import Data.Either (fromRight, rights)


type Column = [Text]
type Columns = [Column]
type Action = (Int, Int, Int)

simulate :: Bool -> Columns -> Action -> Columns
simulate shouldReverse cols (count, from, to)  = do
    let (toMove, remainingFrom) = splitAt count (cols !! (from - 1))
    let newTo = (if shouldReverse then reverse else id) toMove ++ (cols !! (to - 1))
    fmap (subUpdate (from - 1) remainingFrom (to - 1) newTo) (zip [0..] cols) where
            subUpdate fromIndex fromRemains toIndex newTo (ind, el)
                | ind == fromIndex = fromRemains
                | ind == toIndex = newTo
                | otherwise = el

solution input shouldReverse = do
    let rows = lines input
    let (stackRows, actions) = filter (not . Prelude.null) <$> break (== "") rows
    let columns = init . fmap unbracket . filter (not . Data.Text.null) . fmap (strip . pack) <$> transpose (chunksOf 4 <$> stackRows) where
        unbracket :: Text -> Text
        unbracket t = fromJust $ Data.Text.stripPrefix "[" t >>= Data.Text.stripSuffix "]" 
    let actionTuples = (\list -> (fix $ list!!1, fix $ list!!3, fix $ list!!5)) .  Data.Text.splitOn " " . pack <$> actions where
        fix :: Text -> Int
        fix a = fst (fromRight (5, "") (decimal a))
    let finalColumns = foldl (simulate shouldReverse) columns actionTuples
    print $ Data.Text.concat (head <$> finalColumns)

part1 input = solution input True 
part2 input = solution input False 

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input