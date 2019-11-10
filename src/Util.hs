module Util where

    import Graphics.Gloss.Interface.Pure.Game
    import Graphics.Gloss.Data.Bitmap
    import Graphics.Gloss.Data.Picture
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
    initialPlayer :: Player
    initialPlayer = Player (0, 0) 0 0 0 dhb False defaultPlatformRect
    dhb :: Hitbox
    dhb = MkHitbox (Coord 0 0) (Coord 32 32)

    --Types
    type Seed = Float 
    data Platform = MkPlatform (Coord, Coord) Rectangle
    type ActChunks = (Chunk,Chunk,Chunk)  
    type FloatCoord = (Float, Float)

    data Coord = Coord {cx :: Int, cy :: Int}

    instance Num Coord where
      (-) a b = Coord {cx = (cx a - cx b), cy = (cy a - cy b)}
      (+) a b = Coord {cx = (cx a + cx b), cy = (cy a + cy b)}

    data GameState = MkGameState { gChunks     :: ActChunks
                                 , gPlayer     :: Player
                                 , gEnemies    :: [AI]
                                 , gIsPaused   :: Bool
                                 , gSeed       :: Int
                                 , gKeyPresses :: [Event]
                                 , gXOffset    :: Float
                                 , gBitMapData :: BitmapData
                                 , gPossibleChunks :: [Chunk]
                                 , gLoaded     :: Bool
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



    data AI_type = AI1 | AI2 | AI3 --Possible more types if needed
        deriving (Eq)
    data AI = MkAI AI_type FloatCoord Hitbox Rectangle

    data Hitbox = MkHitbox { 
        start :: Coord,   --topleft corner
        end   :: Coord    --bottomright corner
    }

    data Chunk = MkChunk { 
        chunkId        :: Int,
        chunkLength    :: Int,
        chunkUnloadPos :: Int,
        chunkPlatforms :: [Platform]
    }
    
    startingChunks :: ActChunks
    startingChunks = (MkChunk 0 15 30 [MkPlatform (Coord 0 576, Coord 480 640) defaultPlatformRect]
                     ,MkChunk 1 15 45 [MkPlatform (Coord 0 576, Coord 480 640) defaultPlatformRect, MkPlatform (Coord 5 12, Coord 10 12) defaultPlatformRect]
                     ,MkChunk 2 15 60 [MkPlatform (Coord 0 576, Coord 480 640) defaultPlatformRect])
    
    defaultPlatformRect :: Rectangle
    defaultPlatformRect = Rectangle (0,0) (32,32)
    
    --Util functions
    toCoord :: FloatCoord -> Coord
    toCoord (x,y) = Coord {cx = round x, cy = round y}

    toFloatCoord :: Coord -> FloatCoord
    toFloatCoord (Coord a b) = ((intToFloat a),(intToFloat b))

    posSub :: Num a => (a,a) -> (a,a) -> (a,a)
    posSub (a,b) (c,d) = (a - c,b - d)
    
    posAdd :: Num a => (a,a) -> (a,a) -> (a,a)
    posAdd (a,b) (c,d) = (a + c,b + d)

    posMul :: Num a => a -> (a,a) -> (a,a)
    posMul n (a,b) = (n * a,n * b)

    gridToPixels :: Coord -> Coord
    gridToPixels (Coord a b) = Coord (a * 32) (b * 32)

    bottomRight :: Coord -> Coord
    bottomRight (Coord a b) = Coord (a + 32) (b + 32)

    platformHitbox :: Platform -> Hitbox
    platformHitbox (MkPlatform (a,b) _) = MkHitbox pa pbrb
        where
            pa = gridToPixels a
            pbrb = bottomRight (gridToPixels b)

    flipTuple :: (a,b) -> (b,a)
    flipTuple (a,b) = (b,a)

    getPlatforms :: ActChunks -> [Platform]
    getPlatforms (c0,c1,c2) = chunkPlatforms c0 ++ chunkPlatforms c1 ++ chunkPlatforms c2

    --Util coords
    toDrawCoords :: Float -> FloatCoord -> FloatCoord
    toDrawCoords _ a = a
    --toDrawCoords pX (x,y) = ((x - (intToFloat screenWithHalf) - pX), (y - (intToFloat screenHeightHalf) ))
    

    getPrePicture :: Player -> [AI] -> [Platform] -> Float -> [(Float,Float,Rectangle)]
    getPrePicture player enemies platforms pX = [getTupleElement getPlayerPos getPlayerRectangle] ++ (map getEnemyTuple enemies) ++ (map getPlatformTuple platforms)
      where getPlayerRectangle                        = sprite player
            getEnemyTuple (MkAI _ flc _ rect)         = getTupleElement (toDrawCoords pX flc) rect
            getPlatformTuple (MkPlatform (c,_) rect)  = getTupleElement (toDrawCoords pX ((intToFloat (cx c)),(intToFloat (cy c)))) rect
            getPlayerPos                              = toDrawCoords pX (pos player)
            getTupleElement (a,b) rect                = (a,b,rect)

    toPicture :: (Float,Float,Rectangle) -> BitmapData -> Picture
    toPicture (x,y,rect) bmd = Translate x y (BitmapSection rect bmd)
    
    getFrame :: Player -> [AI] -> [Platform] -> BitmapData -> Float -> Picture
    getFrame pl enemies platforms bmp pX = Pictures (map ((flip toPicture) bmp) (getPrePicture pl enemies platforms pX))

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

    updateHitbox :: FloatCoord -> Hitbox -> Hitbox
    updateHitbox fc h = MkHitbox {start = (start h + df), end = (end h + df)}
      where df = (toCoord fc) - (start h)