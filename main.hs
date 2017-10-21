module Main where
import Map
import AAlgorithm

import System.IO

main = do
    file <- readFile "./maps/weights"
    let m = parse file in putStr (show (m))
