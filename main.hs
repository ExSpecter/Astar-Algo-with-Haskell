module Main where
import Map
import AAlgorithm

import System.IO

main = do
    file <- readFile "./maps/difficult1"
    let m = parse file in putStr (toString m (startAlgorithm m))
