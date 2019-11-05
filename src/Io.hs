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
view g = loadBMP "C:\\Users\\JellePrincipaal\\Documents\\Github\\gahuu\\src\\img\\Dirt.bmp"  -- return (viewPure g)

--Pure view function
viewPure :: GameState -> Picture
viewPure gstate = polygon [(gSeed gstate, 0), (0.5, 0.5), (0,0.5), (0, 0) ]

--Loads save
loadSave :: String -> IO GameState
loadSave path = undefined path

--Get the systemtime
getTime :: IO Int
getTime = undefined

--Get the contents of a file TOFIX
--getFile :: IO FilePath -> IO String
--getFile path = readFile (mappend getCurrentDirectory path)
