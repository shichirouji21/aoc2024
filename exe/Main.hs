module Main where

import Data.Text (unpack)
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import Data.List (sort, group)
import Data.Time (getCurrentTime, diffUTCTime)

main :: IO ()
main = do
  start <- getCurrentTime
  day1
  --day2
  --day3
  --day4
  --day5
  --day6
  --day7
  --day8
  --day9
  --day10
  --day11
  --day12
  end <- getCurrentTime
  putStrLn $ "Execution time: " ++ show (diffUTCTime end start)

day1 :: IO ()
day1 = do
  input <- TIO.readFile "data/day1"
  let (x, y) = day1Parser (lines (unpack input))
  putStrLn $ cutePrint "1A" ((solveA x y) :: Int)
  putStrLn $ cutePrint "1B" ((solveB x y) :: Int)
  where
    solveA :: [Int] -> [Int] -> Int
    solveA x y = sum (map abs (zipWith (-) (sort x) (sort y)))
    solveB :: [Int] -> [Int] -> Int
    solveB x y = sum (map (\key -> key * fromMaybe 0 (lookup key ([(head g, length g) | g <- group (sort y)]))) x)

day2 :: IO ()
day2 = do
  input <- TIO.readFile "data/day2e"
  putStrLn $ cutePrint "2A" ((solveA) :: Int)
  putStrLn $ cutePrint "2B" ((solveB) :: Int)
  where
    solveA :: Int
    solveA = 21 
    solveB :: Int
    solveB = 33

-- purity

-- ["3 2", "4 5"] -> [[3, 4], [2, 5]]
day1Parser :: [String] -> ([Int], [Int])
day1Parser i =
  let p = map parse i 
      x = map fst p
      y = map snd p
  in (x, y)
  where
    parse :: String -> (Int, Int)
    parse s =
      let [x, y] = map read (words s)
      in  (x, y)

cutePrint :: (Show a, Num a) => String -> a -> String
cutePrint s n = "Day " ++ s ++ ": " ++ show (n)
