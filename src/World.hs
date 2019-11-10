module World where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Codec.BMP
import Player
import Ai
import Io
--import Render
import LevelGen
import Util

      
-- Initial state of the game
initialState :: GameState
initialState = MkGameState startingChunks initialPlayer [] False 9000 [] 15 undefined [] False
--Render game
renderGame :: GameState -> Picture
renderGame gs = case gIsPaused gs of
    True  -> pauseMenu ["Continue","Exit"]
    False -> running gs

running :: GameState -> Picture
running = undefined

step :: Float -> GameState -> IO GameState
step dT gs | gLoaded gs == False = initGame gs
           | otherwise = do 
  gs_afterPlayer   <- parseInput dT gs
  gs_afterAI      <- stepAI dT gs_afterPlayer
  gs_afterPhysics <- simPhysics dT gs_afterAI
  gs_afterInteractions <- interactions gs_afterPhysics
  return gs_afterInteractions

initGame :: GameState -> IO GameState
initGame gs = do 
  tilesetimg <- (loadFile readBMP "/src/img/tileset.bmp")
  chunks <- getChunks
  let getbmp = either (undefined) (bitmapDataOfBMP) tilesetimg
  pure (gs {gBitMapData = getbmp, gPossibleChunks = chunks, gLoaded = True})

parseInput :: Float -> GameState -> IO GameState
parseInput dT gs = pure gs { gPlayer = foldl updP (gPlayer gs) (gKeyPresses gs), gKeyPresses = []} 
  where 
    lphb (a, b, c) = map (platformHitbox) (chunkPlatforms a) ++ map (platformHitbox) (chunkPlatforms b) ++ map (platformHitbox) (chunkPlatforms c)
    phb = lphb (gChunks gs) -- Platform hitboxes
    ehb = map getEHitbox (gEnemies gs) -- Enemy hitboxes
    getEHitbox (MkAI _ _ hb _) = hb
    updP = updatePlayer phb ehb dT -- Pre-filled in updatePlayer for foldl usage

stepAI :: Float -> GameState -> IO GameState
stepAI dT gs = pure gs

simPhysics :: Float -> GameState -> IO GameState
simPhysics dT gs = pure gs

interactions :: GameState -> IO GameState
interactions gs = pure gs

pauseMenu = undefined
--pauseMenu buttons = Pictures [(Picture pauseBackgroundColor),pauseButtons buttons]
--pauseButtons :: [String] -> Picture
{-pauseButtons = Pictures map mkButton
  where mkButton :: String -> Int -> [Picture] -> [Picture]
        mkButton str y pics = map (Transpose 0 (-(y/2)*numButtons)) buttons 
          where buttons = map (Translate 0 y) (pics:(Text str))
                numButtons = length buttons
                    -}
