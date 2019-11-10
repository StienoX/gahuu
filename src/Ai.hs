{-# language NamedFieldPuns #-}

module Ai where
import Util



--Processes the behavior for the provided ai
processAI :: AI -> Float -> Player -> AI
processAI (MkAI AI1 aiPos hitbox rect) speed player  | posSub aiPos (pos player) < (0,0) = MkAI AI1 (posSub aiPos (speed,0)) hitbox rect
                                                     | otherwise                         = MkAI AI1 (posAdd aiPos (speed,0)) hitbox rect

processAI (MkAI AI2 aiPos hitbox rect) speed player                                      = MkAI AI2 (posMul speed (posAdd (posSub (pos player) aiPos) aiPos)) hitbox rect

processAI (MkAI AI3 aiPos hitbox rect) speed player  | posSub aiPos (pos player) < (0,0) = MkAI AI1 (posAdd aiPos (speed,0)) hitbox rect
                                                     | otherwise                         = MkAI AI1 (posSub aiPos (speed,0)) hitbox rect

--Processes all ai behavior
updateAI :: [AI] -> Float -> Float -> Player -> [AI]
updateAI xs speed deltaT player = map f xs
  where f x = processAI x (speed*deltaT) player

--Linair gravity ai
gravityAI :: AI -> Float -> [Hitbox] -> AI
gravityAI ai@(MkAI AI2 _ _ _) _ _ = ai
gravityAI (MkAI aiType aiPos aiHitbox aiRect) deltaT hitboxes | aiCollided updatedAI hitboxes = (MkAI aiType aiPos aiHitbox aiRect)
                                                              | otherwise                     = updatedAI
  where updatedAI = MkAI aiType (posSub aiPos (0,deltaT*gravStrength)) aiHitbox aiRect

--Checks if ai has collided with a hitbox
aiCollided :: AI -> [Hitbox] -> Bool
aiCollided (MkAI _ _ hitbox _) hitboxes = elem True (map (collision hitbox) hitboxes)