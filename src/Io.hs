{-# language NamedFieldPuns #-}

module Io where 

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
--import Data.Time.Clock  "Hidden package"
import Util

--Get the keypresses from the player
getKeyPress :: Event -> GameState -> IO GameState
getKeyPress e gs = return (gs {keyPresses = keyPresses gs ++ [e]})

--Impure view function
view :: GameState -> IO Picture
view = return . viewPure

--Pure view function
viewPure :: GameState -> Picture
viewPure gstate = undefined

--Loads save
loadSave :: String -> IO GameState
loadSave path = undefined

--Get the systemtime
getTime :: IO Int
getTime = undefined