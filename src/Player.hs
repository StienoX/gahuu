module Player where
import Util
import Graphics.Gloss.Interface.IO.Game



--Makes an hitbox from floats
makeHitboxFloat :: FloatCoord -> FloatCoord -> Hitbox
makeHitboxFloat (x,y) (x2,y2) = MkHitbox Coord {cx = round x, cy = round y} Coord {cx = round x2, cy = round y2}

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
                           | otherwise = error "Unabled to reposition player outside of hitbox, vx or vy not valid"
        playervx           = player { pos = (posSub (pos player) ((vx player),0)), vx = 0}
        playervy           = player { pos = (posSub (pos player) (0,(vy player))), vy = 0}
        playervxy          = player { pos = (posSub (pos player) ((vx player),(vy player))), vx = 0, vy = 0}

--Updates the player position based on the keypresses provided
playerMove :: Event -> Player -> Float -> Player
playerMove (EventKey (Char 'w') Down _ _) player _       | vy player == 0 = player {vprev = 0, vy = (vy player) + jump}
                                                         | otherwise      = player
playerMove (EventKey (Char 'a') Down _ _)  player deltaT = player {pos = (posSub (pos player) ((deltaT*speedPlayer),0)),vx = (deltaT*speedPlayer)}
playerMove (EventKey (Char 'd') Down _ _)  player deltaT = player {pos = (posAdd (pos player) ((deltaT*speedPlayer),0)),vx = (-deltaT*speedPlayer)}
playerMove  _  player _                                  = player

--Checks if player has collided with a hitbox
playerCollided :: Player -> [Hitbox] -> Bool
playerCollided player hitboxes = elem True (map (collision (hitbox player)) hitboxes)

--Updates player based on gravity
playerGravity :: Player -> Float -> [Hitbox] -> Player
playerGravity player deltaT hitboxes | playerCollided updatedPlayer hitboxes = player
                                     | otherwise                             = updatedPlayer
  where updatedPlayer = player {pos = posSub (pos player) (0,deltaT*gravStrength)}

--Checks if player is by an enemy
playerHitEnemy :: Player -> [Hitbox] -> Player
playerHitEnemy player enemies | playerCollided player enemies = player {isDead = True}
                              | otherwise                     = player

--Updates the player
-- Player - Hitboxes - Enemies - Keypresses - deltaT -> Player
updatePlayer :: Player -> [Hitbox] -> [Hitbox] -> Event -> Float -> Player
updatePlayer player hitboxes enemies eventkey deltaT = playerGravity (playerCollision (playerHitEnemy (playerMove eventkey player deltaT) enemies) hitboxes) deltaT hitboxes