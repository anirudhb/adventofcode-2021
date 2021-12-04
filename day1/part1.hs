module Main where

import Data.List

windows :: Int -> [a] -> [[a]]
windows n xs = transpose (take n (tails xs))

numIncreases :: [Int] -> Int
numIncreases xs = foldl go 0 $ windows 2 xs
    where
        go x [a, b] = x + fromEnum (b > a)
        go x _ = x

main :: IO ()
main = readFile "part1.in.txt" >>= print . numIncreases . map read . lines
