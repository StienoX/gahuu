module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Io
import Util
import World

main :: IO ()
main = playIO window background fps initialState view getKeyPress step