{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.List

data Command = Forward Int | Down Int | Up Int

data Submarine = Submarine { hpos :: Int
                           , depth :: Int
                           , aim :: Int }

defaultSub :: Submarine
defaultSub = Submarine { hpos = 0, depth = 0, aim = 0 }

parseCommand :: String -> Command
parseCommand s = let
        [cmd, n'] = words s
        n = read n'
    in
        case cmd of
          "forward" -> Forward n
          "down" -> Down n
          "up" -> Up n
          cmd -> error ("Unknown command: " ++ cmd)

runCommand :: Submarine -> Command -> Submarine
runCommand sub (Forward n) = sub { hpos = hpos sub + n, depth = depth sub + aim sub * n }
runCommand sub (Down n) = sub { aim = aim sub + n }
runCommand sub (Up n) = sub { aim = aim sub - n }

main :: IO ()
main = readFile "part2.in.txt" >>= print . go . foldl' runCommand defaultSub . map parseCommand . lines
    where
        go sub = hpos sub * depth sub
