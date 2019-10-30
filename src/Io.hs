{-# language NamedFieldPuns #-}

module Io where 

import Graphics.Gloss.Interface.Pure.Game
import World
import Util

type Key = Char

--Get the keypresses from the player
getKeyPress :: Event -> GameState -> IO GameState
getKeyPress (EventKey key Down) GameState {keyPresses} = undefined

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
getTime :: IO Float
getTime = undefined