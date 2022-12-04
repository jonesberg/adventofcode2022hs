-- | Day 2
{-# LANGUAGE QuasiQuotes #-}

module Day3
  () where

import           Data.Char                      ( isUpper
                                                , ord
                                                )
import           Data.Set                       ( elems
                                                , fromList
                                                , intersection
                                                )
import           Data.Set.Internal              ( Set )
import           Text.RawString.QQ              ( r )


commonToSet :: [Set Char] -> Set Char
commonToSet = foldr1 intersection

bagsToSet :: [[Char]] -> [Set Char]
bagsToSet = map fromList

listToBags :: [a] -> [[a]]
listToBags item = [a, b]
  where (a, b) = splitAt (length item `div` 2) item

priority :: Char -> Int
priority x = if isUpper x then ord x - 38 else ord x - 96

getItemsInBoth :: String -> [Char]
getItemsInBoth x =
  concatMap (elems . commonToSet . bagsToSet . listToBags) $ lines x

getItemsInAll3 :: String -> [Char]
getItemsInAll3 x = concatMap (elems . commonToSet . bagsToSet) $ groupsOf3 . lines $ x

day3a :: String -> Int
day3a x = sum $ map priority $ getItemsInBoth x

day3b :: String -> Int
day3b x = sum $ map priority $ getItemsInAll3 x

groupsOf3 :: [a] -> [[a]]
groupsOf3 (x:y:z:xs) = [x, y, z] : groupsOf3 xs
groupsOf3 _ = []


testData :: String
testData = [r|vJrwpWtwJgWrhcsFMMfFFhFp
jqHRNqRjqzjGDLGLrsFMfFZSrLrFZsSL
PmmdzqPrVvPwwTWBwg
wMqvLMZHhHMvwLHjbvcjnnSBnvTQFn
ttgJtRGJQctTZtZT
CrZsJsPPZsGzwwsLwLmpwMDw|]



main :: IO ()
main = do
  d <- readFile "data/3/input"
  print $ "Day 3, part a :: " ++ show (day3a d)
  print $ "Day 3, part b :: " ++ show (day3b d)
