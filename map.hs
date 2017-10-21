module Map (
    Node(Node)
    Map,
    fromFile
) where

import System.IO

data Map = Map String
data Tile = Start | Target | Wall | Empty

fromFile :: String -> Map
fromFile file = Map (readFile file)

data Node a = Node {f::Integer, g::Integer, h::Integer, pos::a}
Map :: [[Node]]
