module Util where

    import Graphics.Gloss.Interface.Pure.Game

    type Seed = Float 
    type Platform = (Coord, Coord)   
    type ActChunks = (Chunk,Chunk,Chunk)  
    type FloatCoord = (Float, Float)

    data Coord = Coord {cx :: Int, cy :: Int}

    data GameState = MkGameState { chunks :: ActChunks
                             , player :: Player
                             , enemies :: [AI]
                             , isPaused :: Bool
                             , seed :: Float
                             , keyPresses :: [Event]
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
        deriving (Eq)

    data Hitbox = MkHitbox { 
        start :: Coord,   --topleft corner
        end   :: Coord    --bottomright corner
    }

    data Chunk = MkChunk { 
        chunkId :: Int,
        length :: Int,
        startPos :: Int,
        endPos :: Int,
        unloadPos :: Int,
        platforms :: [Platform]
    }