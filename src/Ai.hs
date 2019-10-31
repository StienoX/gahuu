module AI where
import Util



--Processes the behavior for the provided ai
processAI :: AI -> Float -> Player -> AI
processAI (MkAI AI1 aiPos) speed Player {playerPos = pos} | (aiPos - pos) < (0,0) = MkAI AI1 (aiPos - (speed,0))
                                                          | otherwise             = MKAI AI1 (aiPos + (speed,0))

processAI (MkAI AI2 aiPos) speed Player {playerPos = pos} = MKAI AI2 (speed * (pos - aiPos) + aiPos)

processAI (MkAI AI3 aiPos) speed Player {playerPos = pos} | (aiPos - pos) < (0,0) = MkAI AI1 (aiPos + (speed,0))
                                                          | otherwise             = MKAI AI1 (aiPos - (speed,0))

--Processes all ai behavior
updateAI :: [AI] -> Float -> Player -> [AI]
updateAI xs speed player = map (processAI speed player) xs