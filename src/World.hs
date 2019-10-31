module World where
import Settings
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Player
import AI
import Render
import LevelGen
import Util

      
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
                    
