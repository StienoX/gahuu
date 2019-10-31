module Util where

    import Graphics.Gloss.Interface.Pure.Game

    --Settings
    screenHeight :: Int
    screenHeight = 640
    screenWith :: Int
    screenWith = 480

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

    toCoord :: FloatCoord -> Coord
    toCoord (x,y) = Coord {cx = round x, cy = round y}

    posSub :: Num a => (a,a) -> (a,a) -> (a,a)
    posSub (a,b) (c,d) = (a - c,b - d)
    
    posAdd :: Num a => (a,a) -> (a,a) -> (a,a)
    posAdd (a,b) (c,d) = (a + c,b + d)