module Io where
import LevelGen
type Key = Char

--Get the keypresses from the player
getKeyPresses :: IO [Key]
getKeyPresses = undefined

--Impure view function
view :: GameState -> IO Picture
view = return . viewPure

--Pure view function
viewPure :: GameState -> Picture
viewPure gstate = undefined

--Loads save
loadSave :: String -> IO GameState
loadSave path = undefined

--Get the systemtime
getTime :: IO Float
getTime = undefined