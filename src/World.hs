module World where
import Player
import Ai
import Io
import Graphics.Gloss

data GameState = MkGameState { chunks :: [Chunk]
                             , player :: Player
                             , enemies :: [AI]
                             , isPaused :: Bool
                             , seed :: Seed
                             , deltaT :: Float
                             , keyPresses :: [Key]
                             }        
-- Initial state of the game              
initialState :: GameState
initialState = undefined

                    
