module Main where

import Data.List
import Data.Ord

fromBinary :: [Int] -> Int
fromBinary = foldl' (\x y -> x * 2 + y) 0

leastCommonBitInPosition0 :: [[Int]] -> Int -> Int
leastCommonBitInPosition0 nums bit = let
    commonBit = head $ sortOn length $ group $ sort (transpose nums !! bit)
    in
    if length commonBit * 2 == length nums then 0 else head commonBit

mostCommonBitInPosition1 :: [[Int]] -> Int -> Int
mostCommonBitInPosition1 nums bit = let
    commonBit = head $ sortOn (Down . length) $ group $ sort (transpose nums !! bit)
    in
    if length commonBit * 2 == length nums then 1 else head commonBit

oxygenGeneratorRating :: [[Int]] -> Int
oxygenGeneratorRating nums = fromBinary $ head $ head $ dropWhile ((/=1) . length) $ scanl' doFilter nums [0..minimum (map length nums) - 1]
    where
        doFilter nums' bitNum = let
            mostCommonBit = mostCommonBitInPosition1 nums' bitNum
            in
            filter ((== mostCommonBit) . (!! bitNum)) nums'

co2ScrubberRating :: [[Int]] -> Int
co2ScrubberRating nums = fromBinary $ head $ head $ dropWhile ((/=1) . length) $ scanl' doFilter nums [0..minimum (map length nums) - 1]
    where
        doFilter nums' bitNum = let
            mostCommonBit = leastCommonBitInPosition0 nums' bitNum
            in
            filter ((== mostCommonBit) . (!! bitNum)) nums'

lifeSupportRating :: [[Int]] -> Int
lifeSupportRating nums = oxygenGeneratorRating nums * co2ScrubberRating nums

main :: IO ()
main = readFile "part2.in.txt" >>= print . lifeSupportRating . map (map (\x -> read [x])) . lines
