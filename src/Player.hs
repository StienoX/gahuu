module Player where

data Player = Player { 
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
playerCollision player hitboxes | playerCollided player hitboxes = getUpdatePosPlayer --move player back based on vy/vgrav,vx
                                | otherwise = player
  where getUpdatePosPlayer | playerCollided playervx  hitboxes == False = playervx
                           | playerCollided playervy  hitboxes == False = playervy
                           | playerCollided playervxy hitboxes == False = playervxy
                           | otherwise = Error "Unabled to reposition player outside of hitbox, vx or vy not valid"
        playervx           = player { pos = (pos player - (vx player,0)), vx = 0}
        playervy           = player { pos = (pos player - (0,vy player)), vy = 0}
        playervxy          = player { pos = (pos player - (vx player ,vy player)), vx = 0, vy = 0}

--Updates the player position based on the keypresses provided
playerMove :: EventKey -> Player -> Float -> Player
playerMove ("w",Down,_,_) player deltaT | vy player == 0 = player {vprev = 0, vy = (vy player) + jump}
                                        | otherwise      = player
playerMove ("a",Down,_,_) player deltaT = player {pos = (pos player - (deltaT*speedPlayer),0),vx = deltaT*speedPlayer}
playerMove ("d",Down,_,_) player deltaT = player {pos = (pos player + (deltaT*speedPlayer),0),vx = -deltaT*speedPlayer}
playerMove  _  player _                 = player

--Checks if player has collided with a hitbox
playerCollided :: Player -> [Hitbox] -> Bool
playerCollided player hitboxes = foldl (|| collision (hitbox player)) False hitboxes

playerGravity :: Player -> Float -> [Hitbox] -> Player
playerGravity player deltaT hitboxes | playerCollided updatedPlayer hitboxes 
  where updatedPlayer = player {pos = (pos player - (0,deltaT*gravStrength))}

--Updates the player
-- Player - Hitboxes - Enemies - Keypresses - deltaT -> Player
updatePlayer :: Player -> [Hitbox] -> [Hitbox] -> EventKey -> Float -> Player
updatePlater = undefined
