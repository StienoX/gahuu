module LevelGen where

type Seed = Float
data Chunk = MkChunk Int Int Int Int Int --id length rootPos endPos unloadPos