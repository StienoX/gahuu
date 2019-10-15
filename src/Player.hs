module Player where
import World
import Io

data Player = MkPlayer { 
  pos :: FloatCoord,
  vx :: Float,
  vy :: Float,
  vgrav :: Float,
  hitbox :: Hitbox,
  isDead :: Bool
}
data Hitbox = MkHitbox { 
  start :: Coord,
  end :: Coord 
}

--Makes an hitbox from floats
makeHitboxFloat :: FloatCoord -> FloatCoord -> Hitbox
makeHitboxFloat x y x2 y2 = MkHitbox (round x) (round y) (round x2) (round y2) 

collision :: Hitbox -> Hitbox -> Bool
collision h1 h2 = undefined

--Checks player has collided with any of the hitboxes
playerCollision :: Player -> [Hitbox] -> Player
playerCollision player hitboxes | playerCollided player hitboxes = undefined --move player back based on vy/vgrav,vx
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
