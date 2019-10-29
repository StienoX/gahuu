module LevelGen where
import Player

type Seed = Float
data FloatCoord = (Float, Float)
data Coord = Coord {cx :: Int, cy :: Int}
type Platform = (Coord, Coord)

toCoord :: FloatCoord -> Coord
toCoord (x,y) = (round x, round y)

platformHitbox :: Platform -> Hitbox
platformHitbox platform = MkHitbox platform

data Chunk = MkChunk { 
  chunkId :: Int,
  length :: Int,
  startPos :: Int,
  endPos :: Int,
  unloadPos :: Int,
  platforms :: [Platform]
}

type ActChunks = (Chunk,Chunk,Chunk)

--Returns chunk based on index
getChunk :: ActChunks -> Int -> Chunk
getChunk 0 (x,_,_) = x
getChunk 1 (_,x,_) = x
getChunk 2 (_,_,x) = x
getChunk _ _ = error "Out of range"

--Generates new chunk based of the previous chunk and the seed
genNextChunk :: ActChunks -> Seed -> ActChunks
genNextChunk (_,x,prevChunk) seed = (x,prevChunk,genPlatforms newChunk seed)
  where newChunk = undefined

--Generates new platforms for a new chunk
genPlatforms :: Chunk -> Seed -> Chunk
genPlatforms (MkChunk chkId len rootPos endPos _ []) seed = undefined
genPlatforms x _ = x

--Checks if new chunks need to be generated and unload old chunks
updateChunks :: Player -> ActChunks -> ActChunks
updateChunks player chunks | floor . playerX > (chunkUnloadPos (getChunk 0 chunks)) = updateChunks (genNextChunk chunks)
                           | otherwise = chunks
  where playerPos = pos player
        playerPos :: FloatCoord -> Float   
        playerX (x,_) = x