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



running :: GameState -> Picture
running = undefined

processEvents :: GameState -> GameState
processEvents gs = gs {gEventData = (foldl eventHandler (gEventData gs) (gKeyPresses gs)),gKeyPresses = []}

step :: Float -> GameState -> IO GameState
step dT gs | gLoaded gs == False = initGame gs
           | gIsPaused gs        = pauseMenu gs
           | otherwise = do 
  gs_afterPlayer  <- parseInput dT (processEvents gs)
  gs_afterAI      <- stepAI dT gs_afterPlayer
  gs_afterPhysics <- simPhysics dT gs_afterAI
  gs_afterInteractions <- interactions gs_afterPhysics
  random <- rollDice (length (gPossibleChunks gs_afterInteractions))
  return gs_afterInteractions { gRandom = random}

initGame :: GameState -> IO GameState
initGame gs = do 
  tilesetimg <- (loadFile readBMP "/src/img/tileset.bmp")
  chunks <- getChunks
  let getbmp = either (undefined) (bitmapDataOfBMP) tilesetimg
  pure (gs {gBitMapData = getbmp, gPossibleChunks = chunks, gLoaded = True})

parseInput :: Float -> GameState -> IO GameState
parseInput dT gs = pure gs { gPlayer = (updatePlayer phb ehb (keyDown (gEventData gs)) dT (gPlayer gs))} 
  where 
    lphb (a, b, c) = map (platformHitbox) (chunkPlatforms a) ++ map (platformHitbox) (chunkPlatforms b) ++ map (platformHitbox) (chunkPlatforms c)
    phb = lphb (gChunks gs) -- Platform hitboxes
    ehb = map getEHitbox (gEnemies gs) -- Enemy hitboxes
    getEHitbox (MkAI _ _ hb _) = hb

stepAI :: Float -> GameState -> IO GameState
stepAI dT gs = pure gs {gEnemies = (updateAI (gEnemies gs) speedAI dT (gPlayer gs) ( map platformHitbox (getPlatforms (gChunks gs)))) } 
--updateAI xs speed deltaT player platforms
simPhysics :: Float -> GameState -> IO GameState
simPhysics dT gs = pure (gs {gXOffset = playerX (gPlayer gs)})

interactions :: GameState -> IO GameState
interactions gs | elem 'p' (keyDown (gEventData gs)) = pure (gs {gIsPaused = True})
                | otherwise = pure gs {gChunks = updateChunks gs (gXOffset gs) (gChunks gs) (gRandom gs), gIsPaused = False}

pauseMenu gs = return (processEvents gs)
