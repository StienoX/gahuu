{-# language NamedFieldPuns #-}

module Ai where
import Util
import Data.Maybe
import Numeric.Extra


--Processes the behavior for the provided ai
processAI :: AI -> Float -> Player -> [Hitbox] -> Maybe AI
processAI ai@(MkAI AI1 aiPos hitbox rect) speed player hitboxes | (flipTuple aiPos) < ((-32.0),0)                                                              = Nothing
                                                                | posSub aiPos (pos player) < (0,0) && (aiCollided (moveAI ai posSub speed) hitboxes == False) = Just (moveAI ai posSub speed)
                                                                | aiCollided (moveAI ai posAdd speed) hitboxes == False                                        = Just (moveAI ai posAdd speed)
                                                                | otherwise                                                                                    = Just (ai)

processAI (MkAI AI2 aiPos hitbox rect) speed player _ = Just (updateHitboxAI (MkAI AI2 (posMul speed (posAdd (posSub (pos player) aiPos) aiPos)) hitbox rect))

processAI ai@(MkAI AI3 aiPos hitbox rect) speed player hitboxes | (flipTuple aiPos) < ((-32.0),0)                                                              = Nothing
                                                                | posSub aiPos (pos player) < (0,0) && (aiCollided (moveAI ai posAdd speed) hitboxes == False) = Just (moveAI ai posAdd speed)
                                                                | aiCollided (moveAI ai posSub speed) hitboxes == False                                        = Just (moveAI ai posSub speed)
                                                                | otherwise                                                                                    = Just (ai)
--Processes all ai behavior
updateAI :: [AI] -> Float -> Float -> Player -> [Hitbox] -> [AI]
updateAI xs speed deltaT player platforms = map fgai (mapMaybe fpai xs)
  where fpai x = processAI x (speed*deltaT) player platforms
        fgai x = gravityAI x (gravStrength*deltaT) platforms

--Linair gravity ai
gravityAI :: AI -> Float -> [Hitbox] -> AI
gravityAI ai@(MkAI AI2 _ _ _) _ _ = ai
gravityAI (MkAI aiType aiPos aiHitbox aiRect) deltaT hitboxes | aiCollided updatedAI hitboxes = (MkAI aiType aiPos aiHitbox aiRect)
                                                              | otherwise                     = updatedAI
  where updatedAI = updateHitboxAI (MkAI aiType (posSub aiPos (0,deltaT*gravStrength)) aiHitbox aiRect)

--Checks if ai has collided with a hitbox
aiCollided :: AI -> [Hitbox] -> Bool
aiCollided (MkAI _ _ hitbox _) hitboxes = elem True (map (collision hitbox) hitboxes)

updateHitboxAI :: AI -> AI
updateHitboxAI (MkAI aiType aiPos hitbox rect) = MkAI aiType aiPos (updateHitbox aiPos hitbox) rect

moveAI :: AI -> ((Float,Float) -> (Float,Float) -> (Float,Float)) -> Float -> AI
moveAI ai@(MkAI aiType aiPos hitbox rect) f speed = updateHitboxAI (MkAI aiType (f aiPos (speed,0)) hitbox rect)