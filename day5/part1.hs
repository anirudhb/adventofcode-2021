{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.List.Split

type Point = (Int, Int)

parsePoint :: String -> Point
parsePoint word = let
    [p1, p2] = splitOn "," word
    in
    (read p1, read p2)

data Line = Line Point Point
-- cross-axis-offset, start, end
data Line' = Horizontal Int Int Int | Vertical Int Int Int

parseLine :: String -> Line
parseLine line = let
    [x, _, y] = words line
    in
    Line (parsePoint x) (parsePoint y)

mkLine' :: Line -> Line'
mkLine' (Line (x1, y1) (x2, y2))
  | x1 == x2 = Vertical x1 (min y1 y2) (max y1 y2)
  | y1 == y2 = Horizontal y1 (min x1 x2) (max x1 x2)
  | otherwise = error "Invalid line, not horizontal or vertical"

linePoints :: Line' -> [Point]
linePoints (Horizontal yo x1 x2) = map (, yo) [x1..x2]
linePoints (Vertical xo y1 y2) = map (xo, ) [y1..y2]

go :: [Line'] -> Int
go x = length $ filter (\y -> length y > 1) $ group $ sort (x >>= linePoints)

main :: IO ()
main = readFile "part1.in.txt" >>= print . go . map mkLine' . filter (\(Line (x1, y1) (x2, y2)) -> x1 == x2 || y1 == y2) . map parseLine . lines
