{-# language NamedFieldPuns #-}

module Io where 

import System.Directory
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
--import Graphics.Gloss.Data.Bitmap
--import System.Random
--import Data.Time.Clock  "Hidden package"
import Util

--Get the keypresses from the player
getKeyPress :: Event -> GameState -> IO GameState
getKeyPress e gs = return (gs {gKeyPresses = gKeyPresses gs ++ [e]})

--Impure view function
view :: GameState -> IO Picture
view g = return (viewPure g) -- return (viewPure g)

--Pure view function
viewPure :: GameState -> Picture
viewPure gs = getFrame (gPlayer gs) (gEnemies gs) (getPlatforms (gChunks gs)) (gBitMapData gs) (gXOffset gs)

--Loads save
loadSave :: String -> IO GameState
loadSave path = undefined path

--Get the systemtime
getTime :: IO Int
getTime = undefined

--Load file relative to executable
loadFile :: (String -> IO a) -> String -> IO a
loadFile f path = do
    filepath <- (mappend getCurrentDirectory (pure path))
    f filepath