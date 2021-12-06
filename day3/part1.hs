module Main where

import Data.List
import Data.Ord

fromBinary :: [Int] -> Int
fromBinary = foldl' (\x y -> x * 2 + y) 0

gamma :: [[Int]] -> Int
gamma = fromBinary . map go . transpose
    where
        go = head . head . sortOn length . group . sort

epsilon :: [[Int]] -> Int
epsilon = fromBinary . map go . transpose
    where
        go = head . head . sortOn (Down . length) . group . sort

powerConsumption :: [[Int]] -> Int
powerConsumption xs = gamma xs * epsilon xs

main :: IO ()
main = readFile "part1.in.txt" >>= print . powerConsumption . map (map (\x -> read [x])) . lines
