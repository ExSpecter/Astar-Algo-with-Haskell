module Main where
import Map
import AAlgorithm

import System.IO

main = do
    file <- readFile "./maps/simple"
    putStr (show (startAlgorithm (createMap file)))

createMap :: String -> Map
createMap map = parse map
