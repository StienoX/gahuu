module Ai where
import Player

data AI_type = AI1 | AI2 --Possible more types if needed
  deriving (Eq)
data AI aiType x y = MkAI AI_type Float Float Hitbox
  deriving (Eq)

--Processes the behavior for the provided ai
processAI :: AI -> Float -> Player -> AI
processAI (MkAI AI1 x y) speed player = undefined
processAI (MkAI AI2 x y) speed player = undefined

--Processes all ai behavior
updateAI :: [AI] -> Float -> Player -> [AI]
updateAI xs speed player = map (processAI speed player) xs