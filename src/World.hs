module World where
import LevelGen
import Player
import Ai
import Io
import Settings
import Graphics.Gloss

data GameState = MkGameState { chunks :: ActChunks
                             , player :: Player
                             , enemies :: [AI]
                             , isPaused :: Bool
                             , seed :: Seed
                             , keyPresses :: [Key]
                             }        
-- Initial state of the game
initialState :: GameState
initialState = undefined

--Render game
renderGame :: GameState -> Picture
renderGame gs = case gs isPaused of
    True  -> pauseMenu ["Continue","Exit"]
    False -> running gs

running :: GameState -> Picture
running = undefined

pauseMenu buttons = Pictures [pauseBackgroundColor,pauseButtons buttons]
pauseButtons :: [String] -> Picture
pauseButtons = Pictures map mkButton
  where mkButton :: String -> Int -> [Picture] -> [Picture]
        mkButton str y pics = map (Transpose 0 (-(y/2)*numButtons)) buttons 
          where buttons = map (Translate 0 y) (pics:(Text str))
                numButtons = length buttons
                    
