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
genNextChunk :: GameState -> ActChunks -> Int -> ActChunks
genNextChunk gs (_,x,prevChunk) seed = (x,prevChunk,newChunk)
  where newChunk = selectedChunk {chunkStart = newstart, chunkUnloadPos = newstart + (chunkLength selectedChunk), chunkPlatforms = map (flip (addChunkStart) (newstart)) (chunkPlatforms selectedChunk) }
        selectedChunk = (gPossibleChunks gs) !! (gRandom gs)
        newstart = (chunkStart prevChunk) + (chunkLength prevChunk)

rollDice :: Int -> IO Int
rollDice max = getStdRandom (randomR (1,max))

--Checks if new chunks need to be generated and unload old chunks
updateChunks :: GameState -> Float -> ActChunks -> Int -> ActChunks
updateChunks gs xoffset chunks seed | (floor (xoffset)) > (chunkUnloadPos (getChunk 0 chunks)) = updateChunks gs xoffset (genNextChunk gs chunks seed) seed
                                    | otherwise = chunks