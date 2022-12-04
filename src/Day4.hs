-- | Day 2
{-# LANGUAGE QuasiQuotes #-}

module Day4
  () where

import           Data.Char                      ( isUpper
                                                , ord
                                                )
import           Data.List.Split                ( splitOn )
import           Data.Set                       ( elems
                                                , fromList
                                                , intersection
                                                )
import           Data.Set.Internal              ( Set )
import           Text.RawString.QQ              ( r )

uncurry4 :: (t1 -> t2 -> t3 -> t4 -> t5) -> (t1, t2, t3, t4) -> t5
uncurry4 f (a, b, c, d) = f a b c d

fullyContain :: (Ord a1, Ord a2) => a1 -> a2 -> a1 -> a2 -> Bool
fullyContain a1 a2 b1 b2 | (a1 >= b1) && (a2 <= b2) = True
                         | (b1 >= a1) && (b2 <= a2) = True
                         | otherwise                = False

partiallyContain :: Ord a => a -> a -> a -> a -> Bool
partiallyContain a1 a2 b1 b2 | (b1 <= a1) && (a1 <= b2) = True
                             | (b1 <= a2) && (a2 <= b2) = True
                             | (a1 <= b1) && (b1 <= a2) = True
                             | (a1 <= b2) && (b2 <= a2) = True
                             | otherwise                = False

boundListToTuple :: [[Char]] -> (Int, Int, Int, Int)
boundListToTuple (x : y : _) = (x1, x2, y1, y2)
 where
  (x1 : x2 : _) = map (\x -> read x :: Int) $ splitOn "-" x
  (y1 : y2 : _) = map (\x -> read x :: Int) $ splitOn "-" y

day4 :: (Int -> Int -> Int -> Int -> Bool) -> String -> Int
day4 fullPartial d =
  length
    $ filter (== True)
    $ map (uncurry4 fullPartial . boundListToTuple . splitOn ",")
    $ lines d

testData :: String
testData = [r|2-4,6-8
2-3,4-5
5-7,7-9
2-8,3-7
6-6,4-6
2-6,4-8|]


main :: IO ()
main = do
  d <- readFile "data/4/input"
  print $ "Day 4, part a :: " ++ show (day4 fullyContain d)
  print $ "Day 4, part b :: " ++ show (day4 partiallyContain d)
