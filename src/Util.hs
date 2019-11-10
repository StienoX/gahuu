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
    screenWithHalf :: Float
    screenWithHalf   = (-1*intToFloat (screenWith `div` 2))
    screenHeightHalf :: Float
    screenHeightHalf = (-1*intToFloat (screenHeight `div` 2 - 96))
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
    speedAI :: Float
    speedAI = 120.0
    speedPlayer :: Float
    speedPlayer = 256.0
    jump :: Float
    jump = 6.3
    gravStrength :: Float
    gravStrength = 11.0
    initialPlayer :: Player
    initialPlayer = Player (0, 64) 0 0 0 (MkHitbox (Coord 0 64) (Coord 32 96)) False defaultPlatformRect
    initialEnemies :: [AI]
    initialEnemies = [(MkAI AI3 (400,64) (MkHitbox (Coord 400 64) (Coord 432 96)) defaultPlatformRect)]
    initialState :: GameState
    initialState = MkGameState startingChunks initialPlayer initialEnemies False 9000 [] initEventData 0 undefined [] False
    
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
                                 , gRandom     :: Int
                                 , gKeyPresses :: [Event]
                                 , gEventData  :: EventData
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

    data EventData = EventData {keyDown :: [Char], keyUp :: [Char]}
    eventHandler :: EventData -> Event -> EventData
    eventHandler ev (EventKey (Char 'w') Down _ _) = eventInsertDown ev 'w'
    eventHandler ev (EventKey (Char 'd') Down _ _) = eventInsertDown ev 'd'
    eventHandler ev (EventKey (Char 'a') Down _ _) = eventInsertDown ev 'a'
    eventHandler ev (EventKey (Char 'w') Up _ _)   = eventInsertUp   ev 'w'
    eventHandler ev (EventKey (Char 'd') Up _ _)   = eventInsertUp   ev 'd'
    eventHandler ev (EventKey (Char 'a') Up _ _)   = eventInsertUp   ev 'a'
    eventHandler ev _                              = ev

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
        chunkStart     :: Int,
        chunkUnloadPos :: Int,
        chunkPlatforms :: [Platform]
    }
    
    startingChunks :: ActChunks
    startingChunks = (MkChunk 0 (15 * 32) 0         (30 * 32) [MkPlatform (Coord 0 0, Coord 480 32) defaultPlatformRect]
                     ,MkChunk 1 (15 * 32) (15 * 32) (45 * 32) [MkPlatform (Coord 240 0, Coord 480 32) (Rectangle (0,0) (480,32))]
                     ,MkChunk 2 (15 * 32) (30 * 32) (60 * 32) [MkPlatform (Coord 0 0, Coord 480 32) defaultPlatformRect])
    
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

    playerX :: Player -> Float
    playerX player = getX (pos player)
      where getX (x,_) = x

    addChunkStart :: Platform -> Int -> Platform
    addChunkStart (MkPlatform (a,b) rect) x = MkPlatform ((a + (Coord x 0)), (b + (Coord x 0))) rect

    addChunkStartRect :: Rectangle -> Int -> Rectangle
    addChunkStartRect rect x = rect {rectPos = (posAdd (x,0) (rectPos rect))}

    platformHitbox (MkPlatform (a,b) _) = MkHitbox a b
    {--platformHitbox :: Platform -> Hitbox
    platformHitbox (MkPlatform (a,b) _) = MkHitbox pa pbrb
        where
            pa = gridToPixels a
            pbrb = bottomRight (gridToPixels b)
--}
    flipTuple :: (a,b) -> (b,a)
    flipTuple (a,b) = (b,a)

    getPlatforms :: ActChunks -> [Platform]
    getPlatforms (c0,c1,c2) = chunkPlatforms c0 ++ chunkPlatforms c1 ++ chunkPlatforms c2

    --Util coords
    toDrawCoords :: Float -> FloatCoord -> FloatCoord
    toDrawCoords x (a,b) = (a-x,b)
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
    getFrame pl enemies platforms bmp pX = Translate 0 screenHeightHalf (Pictures (map ((flip toPicture) bmp) (getPrePicture pl enemies platforms pX)))

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

    eventInsertDown :: EventData -> Char -> EventData
    eventInsertDown ev char | elem char (keyDown ev) == False = ev {keyDown = keyDown ev ++ [char], keyUp = filter (/=char) (keyUp ev)}
                            | otherwise = ev
    eventInsertUp   :: EventData -> Char -> EventData
    eventInsertUp ev char   | elem char (keyUp ev) == False =  ev {keyUp = keyUp ev ++ [char], keyDown = filter (/=char) (keyDown ev)}
                            | otherwise = ev

    initEventData :: EventData
    initEventData = EventData [] []
