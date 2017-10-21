module Main where
import Map

import System.IO

main = do
    file <- readFile "./maps/simple"
    putStr (show (createMap file))

createMap :: String -> Map
createMap map = parse map