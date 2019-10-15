module World where
import LevelGen
import Player
import Ai
import Io
import Graphics.Gloss

data GameState = MkGameState { chunks :: ActChunks
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

                    
