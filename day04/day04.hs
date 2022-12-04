{-# LANGUAGE OverloadedStrings #-}

import Data.Text (splitOn, head, last)
import Data.String
import Data.Text.Read
import Data.Either (rights)

part1 input = do
    let asssignments = fmap (fmap fst . rights . fmap decimal . splitOn "-") . splitOn "," . fromString <$> lines input
    let contains = filter eitherContains asssignments where
        eitherContains [p1, p2] = leftInsideRight [p1, p2] || leftInsideRight [p2, p1]
        leftInsideRight [p1, p2] = Prelude.head p1 >= Prelude.head p2 && Prelude.last p1 <= Prelude.last p2
    
    print $ length contains

part2 input = do
    let asssignments = fmap (fmap fst . rights . fmap decimal . splitOn "-") . splitOn "," . fromString <$> lines input
    let overlaps = filter eitherOverlaps asssignments where
        eitherOverlaps [p1, p2] = leftInsideRight [p1, p2] || leftInsideRight [p2, p1]
        leftInsideRight [p1, p2] = Prelude.head p1 `pointInsideInterval` p2 || Prelude.last p1 `pointInsideInterval` p2 
        pointInsideInterval point interval = point >= Prelude.head interval && point <= Prelude.last interval
    print $ length overlaps

main = do
    input <- readFile "input.txt"
    part1 input
    part2 input
