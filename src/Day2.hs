-- | Day 2
{-# LANGUAGE QuasiQuotes #-}

module Day2 () where

import           Data.List                      ( sort )
import           Data.List.Split                ( splitWhen )
import           Text.RawString.QQ              ( r )

data RPS = Rock | Paper | Scissor deriving (Show, Eq)
data Outcome = Win | Draw | Lose deriving (Show, Eq)

data Action =
  R RPS | O Outcome deriving (Show, Eq)

interpretMove :: String -> RPS
interpretMove "A" = Rock
interpretMove "B" = Paper
interpretMove "C" = Scissor
interpretMove "X" = Rock
interpretMove "Y" = Paper
interpretMove "Z" = Scissor
interpretMove _   = undefined

interpretMoveOrOutcome :: String -> Action
interpretMoveOrOutcome "A" = R Rock
interpretMoveOrOutcome "B" = R Paper
interpretMoveOrOutcome "C" = R Scissor
interpretMoveOrOutcome "X" = O Lose
interpretMoveOrOutcome "Y" = O Draw
interpretMoveOrOutcome "Z" = O Win
interpretMoveOrOutcome _   = undefined

runMatch :: RPS -> RPS -> (RPS, Outcome)
-- runMatch opponent me
runMatch Rock    Paper   = (Paper, Win)
runMatch Rock    Scissor = (Scissor, Lose)
runMatch Paper   Rock    = (Rock, Lose)
runMatch Paper   Scissor = (Scissor, Win)
runMatch Scissor Rock    = (Rock, Win)
runMatch Scissor Paper   = (Paper, Lose)
runMatch x       _       = (x, Draw)

runMatchAlt :: Action -> Action -> (RPS, Outcome)
runMatchAlt (R Rock) (O Win) = runMatch Rock Paper
runMatchAlt (R Rock) (O Lose) = runMatch Rock Scissor
runMatchAlt (R Paper) (O Win) = runMatch Paper Scissor
runMatchAlt (R Paper) (O Lose) = runMatch Paper Rock
runMatchAlt (R Scissor) (O Win) = runMatch Scissor Rock
runMatchAlt (R Scissor) (O Lose) = runMatch Scissor Paper
runMatchAlt (R x) (O Draw) = (x, Draw)

rpsToInt :: Num a => RPS -> a
rpsToInt Rock    = 1
rpsToInt Paper   = 2
rpsToInt Scissor = 3

outcomeToInt :: Num a => Outcome -> a
outcomeToInt Win  = 6
outcomeToInt Draw = 3
outcomeToInt Lose = 0

score :: Num a => RPS -> Outcome -> a
score x y = rpsToInt x + outcomeToInt y


day2 :: Num c => (b -> b -> (RPS, Outcome)) -> (String -> b) -> String -> c
day2 match move =
  sum . map
      ( uncurry score
      . uncurry match
      . (\(x : y : _) -> (x, y))
      . map move
      . words
      )
    . lines



testData :: String
testData = [r|A Y
B X
C Z|]



main :: IO ()
main = do
  d <- readFile "data/2/input"
  print $ "Day 2, part a :: " ++ show (day2 runMatch interpretMove d)
  print $ "Day 2, part b :: " ++ show (day2 runMatchAlt interpretMoveOrOutcome d)
