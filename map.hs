module Map (
    Map,
    fromFile
) where

import System.IO 

data Map = Map String
data Tile = Start | Target | Wall | Empty

fromFile :: String -> Map
fromFile file = Map (readFile file)
