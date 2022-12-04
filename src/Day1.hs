-- | Day 1
{-# LANGUAGE QuasiQuotes #-}

module Day1 where

import Text.RawString.QQ ( r )
import Data.List.Split ( splitWhen )
import Data.List ( sort )

getEachElf :: String -> [[String]]
getEachElf input = splitWhen (== "") $ lines input

toListsOfInts :: [[String]] -> [[Int]]
toListsOfInts = map $ map $ \y -> read y :: Int

elfMostCalories :: (Ord b, Foldable t1, Foldable t2, Num b) => t1 (t2 b) -> b
elfMostCalories = foldr (max . sum) 0

day1a :: String -> Int
day1a = elfMostCalories . toListsOfInts . getEachElf

day1b :: String -> Int
day1b = sum . take 3 . reverse . sort . map sum . toListsOfInts . getEachElf

testData :: String
testData = [r|1000
2000
3000

4000

5000
6000

7000
8000
9000

10000|]

main :: IO ()
main = do
  d <- readFile "data/1/input"
  print $ "Day 1, part a :: " ++ show (day1a d)
  print $ "Day 1, part b :: " ++ show (day1b d)
