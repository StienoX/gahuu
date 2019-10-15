module Player where
import World
import Io

data Player = MkPlayer { x :: Float, y :: Float, vx :: Float, vy :: Float, hitbox :: Hitbox, isDead :: Bool}
data Hitbox = MkHitbox {start :: Coord, end :: Coord }

makeHitboxFloat :: Float -> Float -> Float -> Float -> Hitbox
makeHitboxFloat x y x2 y2 = MkHitbox (round x) (round y) (round x2) (round y2) 

collision :: Hitbox -> Hitbox -> Bool
collision h1 h2 = undefined

playerCollision :: Player -> [Hitbox] -> Player
playerCollision player hitboxes = undefined

playerMove :: Key -> Player -> Float -> Player
playerMove 'w' player deltaT = undefined
playerMove 'a' player deltaT = undefined
playerMove 's' player deltaT = undefined
playerMove 'd' player deltaT = undefined
playerMove  _  player _      = player

playerDead :: Player -> [Hitbox] -> Bool
playerDead player hitboxes = foldl (|| collision (playerHitbox player)) False hitboxes

updatePlayer :: Player -> [Hitbox] -> Key -> Float -> Player
updatePlater = undefined
