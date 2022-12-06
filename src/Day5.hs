-- | Day 5
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module Day5
  () where


import           Control.Lens                   ( (&)
                                                , (.~)
                                                , element
                                                )
import           Control.Monad.ST               ( fixST )
import           Data.Char                      ( isSpace
                                                , isUpper
                                                , ord
                                                )
import           Data.List                      ( transpose )
import           Data.Set                       ( elems
                                                , fromList
                                                , intersection
                                                )
import           Data.Set.Internal              ( Set )
import           Text.RawString.QQ              ( r )

newtype Crate = Crate Char deriving (Eq, Show)

strip :: [Char] -> [Char]
strip = dropWhile isSpace

parseCrateRow :: String -> [Maybe Crate]
parseCrateRow []                         = []
parseCrateRow ('[' : x : ']' : ' ' : xs) = Just (Crate x) : parseCrateRow xs
parseCrateRow ('['     : x   : ']' : xs) = Just (Crate x) : parseCrateRow xs
parseCrateRow (' '     : ' ' : ' ' : xs) = Nothing : parseCrateRow xs
parseCrateRow (' '                 : xs) = parseCrateRow xs

grabCrateData :: String -> [String]
grabCrateData = init . takeWhile (/= "") . lines

setupInstructions :: ([String] -> [b]) -> String -> [b]
setupInstructions interpret =
  concatMap (interpret . words)
    . dropWhile (== "")
    . dropWhile (/= "")
    . lines

interpretInstructions :: [[Char]] -> [(Int, Int, Int)]
interpretInstructions ["move", n, "from", from, "to", to] =
  replicate (read n :: Int) (1, read from :: Int, read to :: Int)
interpretInstructions _ = undefined

interpretInstructionsB :: [[Char]] -> [(Int, Int, Int)]
interpretInstructionsB ["move", n, "from", from, "to", to] =
  [(read n :: Int, read from :: Int, read to :: Int)]
interpretInstructionsB _ = undefined

setupCrates :: String -> [[Crate]]
setupCrates = map clean . transpose . map parseCrateRow . grabCrateData
 where
  clean ((Just x) : xs) = x : clean xs
  clean (Nothing  : xs) = clean xs
  clean _               = []

exec :: [[Crate]] -> (Int, Int, Int) -> [[Crate]]
exec crates (n, from, to) =
  let val     = take n (crates !! (from - 1))
      newFrom = drop n (crates !! (from - 1))
      newTo   = val ++ (crates !! (to - 1))
  in  (crates & element (from - 1) .~ newFrom) & element (to - 1) .~ newTo

execAll :: [[Crate]] -> [(Int, Int, Int)] -> [[Crate]]
execAll = foldl exec

solveA :: [[Crate]] -> [Char]
solveA = map ((\(Crate x) -> x) . head)


-- Testing!
ttt :: [[Crate]]
ttt = setupCrates testData
iii :: [(Int, Int, Int)]
iii = setupInstructions interpretInstructions testData

iiib :: [(Int, Int, Int)]
iiib = setupInstructions interpretInstructionsB testData

testData :: String
testData = [r|    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2|]

testData1 :: String
testData1 = [r|    [D]
[N] [C]
[Z] [M] [P]
 1   2   3|]

main :: IO ()
main = do
  d <- readFile "data/5/input"
  let c = setupCrates d
  let i = setupInstructions interpretInstructions d
  let ib = setupInstructions interpretInstructionsB d
  print $ "Day 5, part a :: " ++ show (solveA $ execAll c i)
  print $ "Day 5, part b :: " ++ show (solveA $ execAll c ib)
