module Main where

import Data.Text (unpack)
import qualified Data.Text.IO as TIO
import Data.Maybe (fromMaybe)
import Data.List (sort, group)
import Data.Time (getCurrentTime, diffUTCTime)
import Data.Char (isDigit)
import Data.List (find)
import qualified Data.Map as Map
import Data.Map (Map)

main :: IO ()
main = do
  start <- getCurrentTime
  --day1
  day2
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
  input <- TIO.readFile "data/day2"
  let x = day2Parser (lines (unpack input))
  putStrLn $ cutePrint "2A" ((solveA x) :: Int)
  putStrLn $ cutePrint "2B" ((solveB x) :: Int)
  where
    solveA :: [[Int]] -> Int
    solveA x = length [f | y <- x, let f = applyFilter y, isValid f y]
    solveB :: [[Int]] -> Int
    solveB x = length [y | y <- x, any isValid (y : removeOne y)]
      where
        removeOne :: [Int] -> [[Int]]
        removeOne [] = []
        removeOne (x:xs) = xs : map (x:) (removeOne xs)
        isValid :: [Int] -> Bool
        isValid x = 
          let f = applyFilter x
          in not (null f) && (isAscending f || isDescending f) && length x == length f
    applyFilter :: [Int] -> [Int]
    applyFilter [] = []
    applyFilter (x:xs) = go x xs
      where
        go p [] = [p]
        go p (y:ys)
          | isValidStep p y = p : go y ys
          | otherwise           = go p ys
    isValidStep :: Int -> Int -> Bool
    isValidStep prev z = let diff = z - prev in diff >= 1 && diff <= 3 || diff >= -3 && diff <= -1
    isValid :: [Int] -> [Int] -> Bool
    isValid f o = not (null f) && (isAscending f || isDescending f) && length f == length o

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

-- ["1 2 3 4 5", "6, 7, 8, 9"] -> [[1, 2, 3, 4, 5], [6, 7, 8, 9]]
day2Parser :: [String] -> [[Int]]
day2Parser input = map parse input
  where
    parse :: String -> [Int]
    parse s = map read $ wordsBy (`elem` " ,") s
    wordsBy :: (Char -> Bool) -> String -> [String]
    wordsBy p s = case dropWhile p s of
      "" -> []
      s' -> w : wordsBy p s''
        where (w, s'') = break p s'

isAscending :: [Int] -> Bool
isAscending x = all (> 0) (zipWith (-) (tail x) x)

isDescending :: [Int] -> Bool
isDescending x = all (< 0) (zipWith (-) (tail x) x)

cutePrint :: (Show a, Num a) => String -> a -> String
cutePrint s n = "Day " ++ s ++ ": " ++ show (n)
