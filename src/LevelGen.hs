module LevelGen where
import Util



platformHitbox :: Platform -> Hitbox
platformHitbox (a,b) = MkHitbox a b

--Returns chunk based on index
getChunk :: Int -> ActChunks -> Chunk
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
--genPlatforms (MkChunk chkId len rootPos endPos _ []) seed = undefined
genPlatforms = undefined

--Checks if new chunks need to be generated and unload old chunks
updateChunks :: Player -> ActChunks -> Seed -> ActChunks
updateChunks player chunks seed | (floor (playerX playerPos)) > (chunkUnloadPos (getChunk 0 chunks)) = updateChunks player (genNextChunk chunks seed) seed
                                | otherwise = chunks
  where playerPos = pos player
        playerX :: FloatCoord -> Float   
        playerX (x,_) = x