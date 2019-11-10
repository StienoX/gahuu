module LevelGen where
import Util
import System.Random

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

rollDice :: Int -> IO Int
rollDice max = getStdRandom (randomR (1,max))

--Generates new platforms for a new chunk
genPlatforms :: Chunk -> Seed -> Chunk
--genPlatforms (MkChunk chkId len rootPos endPos _ []) seed = undefined
genPlatforms = undefined

--Checks if new chunks need to be generated and unload old chunks
updateChunks :: Float -> ActChunks -> Seed -> ActChunks
updateChunks xoffset chunks seed | (floor (xoffset)) > (chunkUnloadPos (getChunk 0 chunks)) = updateChunks xoffset (genNextChunk chunks seed) seed
                                 | otherwise = chunks