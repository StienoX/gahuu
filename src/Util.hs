module Util where

    import Graphics.Gloss.Interface.Pure.Game

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
    type Platform = (Coord, Coord)   
    type ActChunks = (Chunk,Chunk,Chunk)  
    type FloatCoord = (Float, Float)

    data Coord = Coord {cx :: Int, cy :: Int}

    data GameState = MkGameState { gChunks     :: ActChunks
                                 , gPlayer     :: Player
                                 , gEnemies    :: [AI]
                                 , gIsPaused   :: Bool
                                 , gSeed       :: Float
                                 , gKeyPresses :: [Event]
                                 , gXOffset    :: Int
                                 }  

    data Player = Player { 
        pos    :: FloatCoord,
        vx     :: Float,
        vy     :: Float,
        vprev  :: Float,
        hitbox :: Hitbox, 
        isDead :: Bool
    }  

    data AI_type = AI1 | AI2 | AI3 --Possible more types if needed
        deriving (Eq)
    data AI = MkAI AI_type FloatCoord Hitbox

    data Hitbox = MkHitbox { 
        start :: Coord,   --topleft corner
        end   :: Coord    --bottomright corner
    }

    data Chunk = MkChunk { 
        chunkId        :: Int,
        chunkLength    :: Int,
        chunkStartPos  :: Int,
        chunkEndPos    :: Int,
        chunkUnloadPos :: Int,
        chunkPlatforms :: [Platform]
    }
    
    --Util functions
    toCoord :: FloatCoord -> Coord
    toCoord (x,y) = Coord {cx = round x, cy = round y}

    posSub :: Num a => (a,a) -> (a,a) -> (a,a)
    posSub (a,b) (c,d) = (a - c,b - d)
    
    posAdd :: Num a => (a,a) -> (a,a) -> (a,a)
    posAdd (a,b) (c,d) = (a + c,b + d)

    posMul :: Num a => a -> (a,a) -> (a,a)
    posMul n (a,b) = (n * a,n * b)

    --Util coords
    toDrawCoords :: Float -> Coord -> Coord
    toDrawCoords pX Coord {cx = x,cy = y} = Coord (x - screenWithHalf - (round pX)) (y - screenHeightHalf) 

