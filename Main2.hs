module Main(main) where

import Graphics.Gloss
import Map
import AAlgorithm

width, height :: Float
width = 40
height = 40

borderCol, wallCol, emptyCol, pathCol :: Color
borderCol = makeColor 0.8 0.8 0.8 1
wallCol = black
emptyCol = white
pathCol = green

getColor :: Node -> Color
getColor (Wall _) = wallCol
getColor (Node _ _ _ _ _) = emptyCol


createMap :: [Node] -> [Picture]
createMap [] = []
createMap (node:rest) = [translate (x * width) (y * height * (-1)) $ color borderCol $ rectangleSolid width height,
                            translate (x * width) (y * height * (-1)) $ color (getColor node) $ rectangleSolid (width - 5) (height - 5)]
                            ++ createMap rest
                            where position = getPos node
                                  y, x :: Float
                                  x = fromIntegral $ fst position
                                  y = fromIntegral $ snd position

createPath :: [Node] -> [Picture]
createPath [] = []
createPath (node:rest) = [translate (x * width) (y * height * (-1)) $ color pathCol $ rectangleSolid (width - 5) (height - 5)]
                            ++ createPath rest
                            where position = getPos node
                                  y, x :: Float
                                  x = fromIntegral $ fst position
                                  y = fromIntegral $ snd position

window :: Display
window = InWindow "A*-Algorithm" (400, 400) (10, 10)

background :: Color
background = white

getNodes2 :: Map -> [Node]
getNodes2 (nodes, _, _) = nodes

drawing :: [Char] -> Picture
drawing file = let m = parse file
                   nodes = getNodes2 m
                   path = startAlgorithm m
               in pictures $ (createMap nodes) ++ (createPath path)

main :: IO ()
main = do
  file <- readFile "./maps/difficult1"
  display window background (drawing file)
