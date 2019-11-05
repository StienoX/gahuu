{-# language NamedFieldPuns #-}

module Ai where
import Util



--Processes the behavior for the provided ai
processAI :: AI -> Float -> Player -> AI
processAI (MkAI AI1 aiPos hitbox) speed player  | posSub aiPos (pos player) < (0,0) = MkAI AI1 (posSub aiPos (speed,0)) hitbox
                                                | otherwise                         = MkAI AI1 (posAdd aiPos (speed,0)) hitbox

processAI (MkAI AI2 aiPos hitbox) speed player                                      = MkAI AI2 (posMul speed (posAdd (posSub (pos player) aiPos) aiPos)) hitbox

processAI (MkAI AI3 aiPos hitbox) speed player  | posSub aiPos (pos player) < (0,0) = MkAI AI1 (posAdd aiPos (speed,0)) hitbox
                                                | otherwise                         = MkAI AI1 (posSub aiPos (speed,0)) hitbox

--Processes all ai behavior
updateAI :: [AI] -> Float -> Player -> [AI]
updateAI xs speed player = map f xs
  where f x = processAI x speed player