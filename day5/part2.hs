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
-- diagonal is just same as normal line
data Line' = Horizontal Int Int Int | Vertical Int Int Int | Diagonal Point Point

parseLine :: String -> Line
parseLine line = let
    [x, _, y] = words line
    in
    Line (parsePoint x) (parsePoint y)

mkLine' :: Line -> Line'
mkLine' (Line p1@(x1, y1) p2@(x2, y2))
  | x1 == x2 = Vertical x1 (min y1 y2) (max y1 y2)
  | y1 == y2 = Horizontal y1 (min x1 x2) (max x1 x2)
  | otherwise = Diagonal p1 p2

linePoints :: Line' -> [Point]
linePoints (Horizontal yo x1 x2) = map (, yo) [x1..x2]
linePoints (Vertical xo y1 y2) = map (xo, ) [y1..y2]
linePoints (Diagonal (x1, y1) (x2, y2)) = let
    dirx = if x2 > x1 then 1 else -1
    diry = if y2 > y1 then 1 else -1
    in
    -- NB: add one so that the last point (x2, y2) is included
    take (abs (x2 - x1) + 1) $ iterate (\(x, y) -> (x + dirx, y + diry)) (x1, y1)

go :: [Line'] -> Int
go x = length $ filter (\y -> length y > 1) $ group $ sort (x >>= linePoints)

main :: IO ()
main = readFile "part2.in.txt" >>= print . go . map (mkLine' . parseLine) . lines
