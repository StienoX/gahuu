module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Io
import Util
import World

window = InWindow "gahuu" (screenHeight,screenWith) (100,100)

background = makeColor 255 255 255 255
fps = 60

main :: IO ()
main = playIO window background fps initialState view getKeyPress step