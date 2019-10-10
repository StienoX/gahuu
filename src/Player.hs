module Player where
import World
import Io

data Player = MkPlayer Float Float Float Float Hitbox Bool --x y vx vy
type Hitbox = ((Int,Int),(Int,Int))

makeHitboxInt   :: Int -> Int -> Int -> Int -> Hitbox
makeHitBoxInt x y x2 y2  = ((x,y)(x2,y2))
makeHitboxFloat :: Float -> Float -> Float -> Float -> Hitbox
makeHitboxFloat x y x2 y2 = makeHitBoxInt (round x) (round y) (round x2) (round y2) 

collision :: Hitbox -> Hitbox -> Bool
collision h1 h2 = undefined

--Gets the players x coord
playerX :: Player -> Float
playerX (MkPlayer x _ _ _ _ _) = x

playerHitbox :: Player -> Hitbox
playerHitbox (MkPlayer _ _ _ _ x _) = x

playerCollision :: Player -> [Hitbox] -> Player
playerCollision player chunks platforms = undefined

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
