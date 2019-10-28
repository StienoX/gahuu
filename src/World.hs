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


pauseMenu = undefined
pauseBackground :: Picture
pauseBackground = undefined
pauseButtons :: [String] -> Picture
pauseButtons = pictures map mkButton
  where mkButton :: String -> [Picture] -> [Picture]
        mkButton str pics = pics:(Text str) -- Moet nog coords krijgen geen idee nog hoe
                    
