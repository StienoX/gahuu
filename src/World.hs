module World where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Player
import Ai
--import Render
import LevelGen
import Util

      
-- Initial state of the game
initialState :: GameState
initialState = MkGameState Nothing Nothing Nothing Nothing 0.5 Nothing

--Render game
renderGame :: GameState -> Picture
renderGame gs = case gIsPaused gs of
    True  -> pauseMenu ["Continue","Exit"]
    False -> running gs

running :: GameState -> Picture
running = undefined

pauseMenu = undefined
--pauseMenu buttons = Pictures [(Picture pauseBackgroundColor),pauseButtons buttons]
--pauseButtons :: [String] -> Picture
{-pauseButtons = Pictures map mkButton
  where mkButton :: String -> Int -> [Picture] -> [Picture]
        mkButton str y pics = map (Transpose 0 (-(y/2)*numButtons)) buttons 
          where buttons = map (Translate 0 y) (pics:(Text str))
                numButtons = length buttons
                    -}
