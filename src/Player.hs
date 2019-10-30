module Player where

data Player = MkPlayer { 
  pos    :: FloatCoord,
  vx     :: Float,
  vy     :: Float,
  vprev  :: Float,
  hitbox :: Hitbox,
  isDead :: Bool
}
data Hitbox = MkHitbox { 
  start :: Coord,   --topleft corner
  end   :: Coord    --bottomright corner
}

--Makes an hitbox from floats
makeHitboxFloat :: FloatCoord -> FloatCoord -> Hitbox
makeHitboxFloat x y x2 y2 = MkHitbox (round x) (round y) (round x2) (round y2) 

collision :: Hitbox -> Hitbox -> Bool
collision h1 h2 | h2x2 < h1x1 = False
                | h2y2 < h1y1 = False
                | h2x1 > h1x2 = False
                | h2y1 > h1y2 = False 
                | otherwise   = True
  where 
    h1x1 = h' cx start h1
    h1y1 = h' cy start h1
    h1x2 = h' cx end   h1
    h1y2 = h' cy end   h1
    h2x1 = h' cx start h2
    h2y1 = h' cy start h2
    h2x2 = h' cx end   h2
    h2y2 = h' cy end   h2
    h' cf nf h = cf (nf h)

--Checks player has collided with any of the hitboxes
playerCollision :: Player -> [Hitbox] -> Player
playerCollision player hitboxes | playerCollided player hitboxes = player { pos = (((fst (player pos)) - (player vy)), ((snd (player pos)) - (player vy)))} --move player back based on vy/vgrav,vx
                                | otherwise = player

--Updates the player position based on the keypresses provided
playerMove :: Key -> Player -> Float -> Player
playerMove 'w' player deltaT = undefined
playerMove 'a' player deltaT = undefined
playerMove 's' player deltaT = undefined
playerMove 'd' player deltaT = undefined
playerMove  _  player _      = player

--Checks if player has collided with a hitbox
playerCollided :: Player -> [Hitbox] -> Bool
playerCollided player hitboxes = foldl (|| collision (hitbox player)) False hitboxes

--Updates the player
-- Player - Hitboxes - Enemies - Keypresses - deltaT -> Player
updatePlayer :: Player -> [Hitbox] -> [Hitbox] -> Key -> Float -> Player
updatePlater = undefined
