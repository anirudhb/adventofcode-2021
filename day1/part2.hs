module Main where

{- windows is from part 1 -}

import Data.List

windows :: Int -> [a] -> [[a]]
windows n xs = transpose (take n (tails xs))

numIncreases :: [Int] -> Int
numIncreases xs = foldl go 0 $ windows 2 $ windows 3 xs
    where
        go x [a, b] = x + fromEnum (sum b > sum a)
        go x _ = x


main :: IO ()
main = readFile "part2.in.txt" >>= print . numIncreases . map read . lines
