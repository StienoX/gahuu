module Util where

    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Data.Bitmap
    import Numeric.Extra

    --Settings
    screenHeight :: Int
    screenHeight = 640
    screenWith :: Int
    screenWith = 480
    screenWithHalf :: Int
    screenWithHalf   = (screenWith `div` 2)
    screenHeightHalf :: Int
    screenHeightHalf = (screenHeight `div` 2)
    windowName :: String
    windowName = "gahuu"
    window :: Display
    window = InWindow windowName (screenHeight,screenWith) (100,100)
    background :: Color
    background = makeColor 255 255 255 255
    fps :: Int
    fps = 60
    backgroundColor :: Color
    backgroundColor = blue
    pauseBackgroundColor :: Color
    pauseBackgroundColor = greyN 0.4
    speedPlayer :: Float
    speedPlayer = 1.0
    jump :: Float
    jump = 1.0
    gravStrength :: Float
    gravStrength = 1.0

    --Types
    type Seed = Float 
    data Platform = MkPlatform (Coord, Coord) Rectangle
    type ActChunks = (Chunk,Chunk,Chunk)  
    type FloatCoord = (Float, Float)

    data Coord = Coord {cx :: Int, cy :: Int}

    data GameState = MkGameState { gChunks     :: ActChunks
                                 , gPlayer     :: Player
                                 , gEnemies    :: [AI]
                                 , gIsPaused   :: Bool
                                 , gSeed       :: Int
                                 , gKeyPresses :: [Event]
                                 , gXOffset    :: Int
                                 , gBitMapData :: BitmapData
                                 , gPossibleChunks :: [Chunk]
                                 }  

    data Player = Player { 
        pos    :: FloatCoord,
        vx     :: Float,
        vy     :: Float,
        vprev  :: Float,
        hitbox :: Hitbox, 
        isDead :: Bool,
        sprite :: Rectangle
    }  

    initialPlayer :: Player
    initialPlayer = Player (15, 15) 0 0 0 dhb False defaultPlatformRect

    data AI_type = AI1 | AI2 | AI3 --Possible more types if needed
        deriving (Eq)
    data AI = MkAI AI_type FloatCoord Hitbox Rectangle

    data Hitbox = MkHitbox { 
        start :: Coord,   --topleft corner
        end   :: Coord    --bottomright corner
    }
    
    dhb :: Hitbox
    dhb = MkHitbox (Coord 15 15) (Coord 15 14)

    data Chunk = MkChunk { 
        chunkId        :: Int,
        chunkLength    :: Int,
        chunkStartPos  :: Int,
        chunkEndPos    :: Int,
        chunkUnloadPos :: Int,
        chunkPlatforms :: [Platform]
    }
    
    startingChunks :: ActChunks
    startingChunks = (MkChunk 0 15 0  15 30 [MkPlatform (Coord 0 18, Coord 15 20) defaultPlatformRect]
                     ,MkChunk 1 15 15 30 45 [MkPlatform (Coord 0 18, Coord 15 20) defaultPlatformRect, MkPlatform (Coord 5 12, Coord 10 12) defaultPlatformRect]
                     ,MkChunk 2 15 30 45 60 [MkPlatform (Coord 0 18, Coord 15 20) defaultPlatformRect])
    
    defaultPlatformRect :: Rectangle
    defaultPlatformRect = Rectangle (0,0) (32,32)
    
    --Util functions
    toCoord :: FloatCoord -> Coord
    toCoord (x,y) = Coord {cx = round x, cy = round y}

    posSub :: Num a => (a,a) -> (a,a) -> (a,a)
    posSub (a,b) (c,d) = (a - c,b - d)
    
    posAdd :: Num a => (a,a) -> (a,a) -> (a,a)
    posAdd (a,b) (c,d) = (a + c,b + d)

    posMul :: Num a => a -> (a,a) -> (a,a)
    posMul n (a,b) = (n * a,n * b)

    platformHitbox :: Platform -> Hitbox
    platformHitbox (MkPlatform (a,b) _) = MkHitbox a b

    --Util coords
    toDrawCoords :: Float -> Coord -> Coord
    toDrawCoords pX Coord {cx = x,cy = y} = Coord (x - screenWithHalf - (round pX)) (y - screenHeightHalf) 

    getRectangles :: Player -> [AI] -> [Platform] -> [(Float,Float,Rectangle)]
    getRectangles player enemies platforms = [getTupleElement getPlayerPos getPlayerRectangle] ++ (map getEnemyTuple enemies) ++ (map getPlatformTuple platforms)
      where getPlayerRectangle                        = sprite player
            getEnemyTuple (MkAI _ flc _ rect)         = getTupleElement flc rect
            getPlatformTuple (MkPlatform (c,_) rect)  = (intToFloat (cx c), intToFloat (cy c),rect)
            getPlayerPos                              = pos player
            getTupleElement (a,b) rect                = (a,b,rect)

    collision :: Hitbox -> Hitbox -> Bool
    collision h1 h2 | h2x2 < h1x1 = False
                    | h2y2 < h1y1 = False
                    | h2x1 > h1x2 = False
                    | h2y1 > h1y2 = False 
                    | otherwise   = True
        where 
        h1x1 = h' cx start h1
        h1y1 = h' cy start h1
        h1x2 = h' cx end   h1
        h1y2 = h' cy end   h1
        h2x1 = h' cx start h2
        h2y1 = h' cy start h2
        h2x2 = h' cx end   h2
        h2y2 = h' cy end   h2
        h' cf nf h = cf (nf h)