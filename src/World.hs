module World where

data World = MkWorld { chunks :: [Chunk]
                     , player :: Player
                     , enemies :: [AI]
                     , isPaused :: Bool
                     , seed :: Seed
                     } 

