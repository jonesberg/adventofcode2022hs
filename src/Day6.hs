-- | Day 2
{-# LANGUAGE QuasiQuotes #-}

module Day3
  () where

import           Data.Char                      ( isUpper
                                                , ord
                                                )
import qualified Data.Set                      as S
import           Data.Set.Internal              ( Set )
import           Text.RawString.QQ              ( r )

allDifferent :: Ord a => Int -> [a] -> Bool
allDifferent n x = S.size (S.fromList x) == n


processPacket :: Ord a => Int -> [a] -> Maybe Int
processPacket n p = processPacket' n n p
 where
  processPacket' i n [] = Nothing
  processPacket' i n p  = if allDifferent n (take n p)
    then Just i
    else processPacket' (i + 1) n (tail p)


t1 = "bvwbjplbgvbhsrlpgdmjqwftvncz"
t2 = "nppdvjthqldpwncqszvftbrmjlhg"
t3 = "nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"
t4 = "zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"


main :: IO ()
main = do
  d <- readFile "data/6/input"
  print "fun"
  print $ "Day 6, part a :: " ++ show (processPacket 4 d)
  print $ "Day 6, part b :: " ++ show (processPacket 14 d)
