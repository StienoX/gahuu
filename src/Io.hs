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
view g | gLoaded g == False = return (rectangleSolid 25.00 25.00)
       | otherwise          = return (viewPure g)

--Pure view function
viewPure :: GameState -> Picture
viewPure gs = getFrame (gPlayer gs) (gEnemies gs) (getPlatforms (gChunks gs)) (gBitMapData gs) (gXOffset gs)

--Loads save
loadSave :: String -> IO GameState
loadSave path = undefined path

--Get the systemtime
getTime :: IO Int
getTime = undefined

getChunks :: IO [Chunk]
getChunks = do 
    files <- loadFile listDirectory "/src/chunks"
    sequence (loader 0 files)

loader :: Int -> [String] -> [IO Chunk]
loader id [] = []
loader id (x:xs) = [loadChunk x id] ++ loader (id+1) xs

loadChunk :: String -> Int -> IO Chunk
loadChunk path id = do
    filec <- loadFile readFile ("/src/chunks/" ++ path)
    let l = length (head (lines filec))
    let chunkhelper [] _ _ platforms = platforms
    let chunkhelper (x:xs) cx cy platforms | x == '#'  = chunkhelper xs (cx + 1) cy [MkPlatform (Coord cx cy, Coord cx cy) defaultPlatformRect] ++ platforms
                                           | x == '\n' = chunkhelper xs 0 (cy + 1) platforms
                                           | otherwise = chunkhelper xs (cx + 1) cy platforms
    pure (MkChunk id (l * 32) 0 (l + 20) (chunkhelper filec 1 1 []))

--Load file relative to executable
loadFile :: (String -> IO a) -> String -> IO a
loadFile f path = do
    filepath <- (mappend getCurrentDirectory (pure path))
    f filepath