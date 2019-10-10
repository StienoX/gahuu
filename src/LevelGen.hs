module LevelGen where
import Player

type Seed = Float
type Platform = Int Int Int Int
platformHitbox :: Platform -> Hitbox
platformHitbox platform = getHitboxInt platform

data Chunk = MkChunk Int Int Int Int Int [Platform] --id length rootPos endPos unloadPos [Platform]

chunkId        :: Int 
chunkId        (MkChunk x _ _ _ _ _) = x
chunkLen       :: Int 
chunkLen       (MkChunk _ x _ _ _ _) = x
chunkRootPos   :: Int 
chunkId        (MkChunk _ _ x _ _ _) = x
chunkEndPos    :: Int 
chunkEndPos    (MkChunk _ _ _ x _ _) = x
chunkUnloadPos :: Int
chunkUnloadPos (MkChunk _ _ _ _ x _) = x
chunkPlatforms :: [Platform]
chunkPlatforms (MkChunk _ _ _ _ _ x) = x

type ActChunks = (Chunk,Chunk,Chunk)

--Returns chunk based on index
getChunk :: ActChunks -> Int -> Chunk
getChunk 0 (x,_,_) = x
getChunk 1 (_,x,_) = x
getChunk 2 (_,_,x) = x
getChunk _ _ = error

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
updateChunks player chunks | floor . playerX player > (chunkUnloadPos (getChunk 0 chunks)) = updateChunks (genNextChunk chunks)
                           | otherwise = chunks