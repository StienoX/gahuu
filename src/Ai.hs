module Ai where

data AI_type = AI1 | AI2 --Possible more types if needed
  deriving (Eq)
data AI aiType x y = MkAI AI_type Float Float 
  deriving (Eq)

processAI :: AI -> Float -> Float -> Player -> AI
processAI (MkAI AI1 x y) speed difficulty player = undefined
processAI (MkAI AI2 x y) speed difficulty player = undefined