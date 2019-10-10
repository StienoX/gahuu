module Highscore where

data HighscoreTable = MkHighscoreTable (Int,Int,Int,Int,Int)

getLowestScore :: HighscoreTable -> Int
getLowestScore (MkHighscoreTable winnerTuple) = lowest winnerTuple
  where lowest (_,_,_,_,x) = x
  