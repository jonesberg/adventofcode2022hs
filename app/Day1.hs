-- | Day 1
{-# LANGUAGE QuasiQuotes #-}

module Day1 where

import Text.RawString.QQ
import Data.List.Split

getEachElf :: String -> [[String]]
getEachElf input = splitWhen (== "") $ lines input


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
main = print "something"
