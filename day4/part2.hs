{-# LANGUAGE TupleSections #-}

module Main where

import Data.List
import Data.List.Split

type BingoDraws = [Int]

parseBingoDraws :: String -> BingoDraws
parseBingoDraws = map read . splitOn ","

type BingoBoard = [[Int]]

parseBingoBoard :: String -> BingoBoard
parseBingoBoard = map parseLine . lines
    where
        parseLine = map read . words

type Bingo = (BingoDraws, [BingoBoard])

parseInput :: String -> Bingo
parseInput input = let x:xs = splitOn "\n\n" input
    in (parseBingoDraws x, map parseBingoBoard xs)

type BingoBoard' = (BingoBoard, [[Bool]])

makeMirror :: BingoBoard -> BingoBoard'
makeMirror board = (board, map (map (const False)) board)

bingoWin :: BingoBoard' -> Bool
bingoWin (_, marked) = check marked || check marked'
    where
        check = any and
        marked' = transpose marked

stepBingo :: BingoBoard' -> Int -> BingoBoard'
stepBingo (board, marked) num = (board, modify board marked)
    where
        modify board marked = zipWith fixupRow board marked
        fixupRow row markedRow = zipWith fixup row markedRow
        fixup num' m = m || (num' == num)

calculateScore :: BingoBoard' -> Int -> Int
calculateScore (board, marked) called = called * sum (diff board marked)
    where
        diff board marked = map fst $ filter (not . snd) $ zip (concat board) (concat marked)

-- Above copied from part1

-- Returns the score of the bingo board that wins last,
-- and if multiple win simultaneously, the one with the highest score
runBingos :: [BingoBoard'] -> BingoDraws -> Int
runBingos boards nums = uncurry calculateScore $ last $ filter (bingoWin . fst) $ fixup $ reduceUntil (\(x, _, _) -> null x) stepMany (boards, [], 0) nums
    where
        fixup (_, winning, n) = map (, n) winning
        reduceUntil cond f init x = head $ dropWhile (not . cond) $ scanl' f init x
        stepMany (boards, winning, _) num = let
                (winning', boards') = partition bingoWin $ map (`stepBingo` num) boards
            in (boards', winning ++ winning', num)

main :: IO ()
main = readFile "part2.in.txt" >>= print . go . parseInput
    where
        go (draws, boards) = runBingos (map makeMirror boards) draws
