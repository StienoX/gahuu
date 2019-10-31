module Main where
import Graphics.Gloss
import Util
import World

window = InWindow "gahuu" (screenHeight,screenWith) (100,100)

background = makeColor 255 255 255 255
fps = 60

main :: IO ()
main = play window background fps initialState gameAsPicture transfromGame animationFunction